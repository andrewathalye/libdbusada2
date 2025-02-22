pragma Ada_2012;

with Ada.Containers.Indefinite_Hashed_Maps;
with Ada.Directories;
with Ada.Environment_Variables;
with Ada.Exceptions;
with Ada.Strings.Fixed;
with Ada.Strings.Hash;
with Ada.Strings.Unbounded;
with Ada.Text_IO;

with Ada.Unchecked_Conversion;
with GNAT.OS_Lib;

with GNATCOLL.OS.FS;
with GNATCOLL.OS.Process;

with D_Bus.Platform;
with D_Bus.Encodings;

function D_Bus.Connection.Parse_Address
  (Mode : Mode_Type; Addr : Server_Address) return GNAT.Sockets.Socket_Set_Type
is
   use GNAT.Sockets;

   type Server_Transport is
     (Unix, Launchd, Systemd, Tcp, Unixexec, Autolaunch);
   --  These are the server transport protocols that we support.
   --  Nonce-Tcp is not supported.

   --  TODO handle connect failure
   function Try_Address (Addr : String) return GNAT.Sockets.Socket_Type;
   function Try_Address (Addr : String) return GNAT.Sockets.Socket_Type is
      use Ada.Strings.Unbounded;
      use D_Bus.Encodings;

      -------------------
      -- Early Renames --
      -------------------
      Colon : constant Positive := Ada.Strings.Fixed.Index (Addr, ":");
      Transport_Name  : String renames Addr (Addr'First .. Colon - 1);
      Transport_Props : String renames Addr (Colon + 1 .. Addr'Last);

      function Key_Value_Start (Key : String) return Natural;
      --  Returns the index into Transport_Props at which the key data starts,
      --  or 0 if the key was not found

      function Key_Value_Start (Key : String) return Natural is
         Key_Name_Start : Positive := Transport_Props'First;
         Equal_Pos   : Natural;
      begin
         Iterate_Key_Names :
         while Key_Name_Start < Transport_Props'Last loop
            --  Search for the end of the current key
            Equal_Pos :=
              Ada.Strings.Fixed.Index
                (Source => Transport_Props, Pattern => "=",
                 From   => Key_Name_Start);

            --  For a value to exist, '=' must be present
            if Equal_Pos = 0 then
               exit Iterate_Key_Names;
            elsif Equal_Pos = Transport_Props'Last then
               raise Address_Error with "Key with no value found";
            end if;

            --  Check against search query
            --  Note we can’t pre-encode the key because
            --  of 'optionally-escaped bytes'
            if Decode_Server_Address
                (Transport_Props (Key_Name_Start .. Equal_Pos - 1)) =
              Key
            then
               return Equal_Pos + 1;
            end if;

            --  Otherwise update Key_Name_Start
            --  Try to find the next ','
            if Ada.Strings.Fixed.Index (Transport_Props, ",", Equal_Pos) = 0
            then
               exit Iterate_Key_Names;
            else
               Key_Name_Start :=
                 Ada.Strings.Fixed.Index (Transport_Props, ",", Equal_Pos) +
                 1;
            end if;
         end loop Iterate_Key_Names;

         --  Key not found
         return 0;
      end Key_Value_Start;

      function Get_Value (Key : String) return String;
      function Get_Value (Key : String) return String is
         V_Start : constant Positive := Key_Value_Start (Key);
         V_End   : Natural;
      begin
         --  Search for the end of the current value
         --  First try searching for the next-key-in-list indicator ','
         V_End :=
           Ada.Strings.Fixed.Index
             (Source => Transport_Props, Pattern => ",", From => V_Start);

         if V_End = 0 then
            V_End := Transport_Props'Last;
         end if;

         return Decode_Server_Address (Transport_Props (V_Start .. V_End));
      end Get_Value;

      function Has_Key (Key : String) return Boolean is
        (Key_Value_Start (Key) /= 0);
      --  Return True if Transport_Props contains the given key,
      --  and False if it is not present.

      function Random_Filename return String;
      function Random_Filename return String
      is (D_Bus.Encodings.To_Hex (String (New_UUID)));

      ----------
      -- Data --
      ----------
      Transport       : Server_Transport;
      Family          : Family_Type;
      Address         : Sock_Addr_Type;
      --  Note Compiler will warn if it is possible for these to be
      --  uninitialised.
   begin
      --  Try to parse the transport
      begin
         Transport := Server_Transport'Value (Transport_Name);
      exception
         when Constraint_Error =>
            raise Transport_Error
              with "Transport '" & Transport_Name & "' is not recognised.";
      end;

      --  Create a socket based upon the transport method
      case Transport is
         when Unix =>
            Family := Family_Unix;

            if Has_Key ("abstract") then
               --  This is valid syntax, just not supported
               raise Transport_Error
                 with "Transport 'Unix' does not support abstract sockets.";
            end if;

            case Mode is
               when Connect =>
                  if not Has_Key ("path") then
                     raise Address_Error
                       with "Connectable transport 'unix' requires 'path'.";
                  end if;

                  Address := Unix_Socket_Address (Get_Value ("path"));
               when Listen =>
                  if Has_Key ("path") then
                     Address := Unix_Socket_Address (Get_Value ("path"));
                  elsif Has_Key ("dir") then
                     Address :=
                       Unix_Socket_Address
                         (Get_Value ("dir") & "/" & Random_Filename);
                  elsif Has_Key ("tmpdir") then
                     Address :=
                       Unix_Socket_Address
                         (Get_Value ("tmpdir") & "/" & Random_Filename);
                  elsif Has_Key ("runtime") then
                     if Get_Value ("runtime") /= "yes" then
                        raise Address_Error
                          with "Listenable transport 'unix' takes" &
                          " 'runtime=yes'";
                     end if;

                     if not Ada.Environment_Variables.Exists
                         ("XDG_RUNTIME_DIR")
                     then
                        raise Transport_Error
                          with "XDG_RUNTIME_DIR not defined and" &
                          " 'runtime=yes' requested.";
                     end if;

                     Address :=
                       Unix_Socket_Address
                         (Ada.Environment_Variables.Value ("XDG_RUNTIME_DIR") &
                          "/" & Random_Filename);
                  else
                     raise Address_Error
                       with "Listenable transport 'unix' requires options";
                  end if;
            end case;

         when Launchd =>
            Family := Family_Unix;

            if not Has_Key ("env") then
               raise Address_Error
                 with "Transport 'launchd' requires option 'env'.";
            end if;

            --  Note:
            --  "env" - per documentation, contains the name of an
            --   environment variable that then contains the real
            --   socket path.
            declare
               Env : constant String := Get_Value ("env");
            begin
               case Mode is
                  when Connect =>
                     --  Retrieve Socket using "launchctl getenv <...>"
                     declare
                        Result : Unbounded_String;
                        Args   : GNATCOLL.OS.Process.Argument_List;
                        Status : Integer;
                     begin
                        --  Blocking `Run` until error or completion
                        Args.Append ("launchctl");
                        Args.Append ("getenv");
                        Args.Append ("Env");

                        Result :=
                          GNATCOLL.OS.Process.Run
                            (Args => Args, Status => Status);

                        --  Check command success
                        if Status /= 0 then
                           raise Transport_Error
                             with "Connectable transport 'launchd' failed" &
                             " to get socket path.";
                        end if;

                        --  TODO do we have to worry about newlines’
                        Address := Unix_Socket_Address (To_String (Result));
                     end;

                  when Listen =>
                     if not Ada.Environment_Variables.Exists (Env) then
                        raise Transport_Error
                          with "Listenable transport 'launchd' must be" &
                          " launched via launchd.";
                     end if;

                     Address :=
                       Unix_Socket_Address
                         (Ada.Environment_Variables.Value (Env));
               end case;
            end;

         when Systemd =>
            Family := Family_Unix;

            case Mode is
               when Connect =>
                  raise Transport_Error
                    with "Transport 'systemd' is not connectable.";

               when Listen =>
                  if not Ada.Environment_Variables.Exists ("LISTEN_FDS") then
                     raise Transport_Error
                       with "Listenable transport 'systemd' must be" &
                       " launched via systemd.";
                  end if;

                  if Ada.Environment_Variables.Value ("LISTEN_FDS") /= "1" then
                     raise Transport_Error
                       with "Listenable transport 'systemd' requires one fd.";
                  end if;

                  --  The first FD systemd uses is '3'
                  return GNAT.Sockets.To_Ada (3);
            end case;

         when Tcp =>
            if not Has_Key ("host") then
               raise Address_Error with "Transport 'tcp' requires 'host'";
            end if;

            if not Has_Key ("port") then
               raise Address_Error with "Transport 'tcp' requires 'port'";
            end if;

            if Has_Key ("family") then
               if Get_Value ("family") = "ipv4" then
                  Family := Family_Inet;
               elsif Get_Value ("family") = "ipv6" then
                  Family := Family_Inet6;
               else
                  raise Address_Error
                    with "Transport 'tcp' does not accept specified 'family'";
               end if;
            else
               --  One last attempt to identify IPv6 addresses
               if GNAT.Sockets.Is_IPv6_Address (Get_Value ("host")) then
                  Family := Family_Inet6;
               else
                  --  No family specified, IPv4 will have to do
                  Family := Family_Inet;
               end if;
            end if;

            --  Connect / Listen / Accept
            --  TODO see if this works with '*' as per spec
            declare
               Host : constant String    := Get_Value ("host");
               Port : constant Port_Type :=
                 Port_Type'Value (Get_Value ("port"));
            begin
               case Mode is
                  when Connect =>
                     Address :=
                       GNAT.Sockets.Network_Socket_Address
                         (Addr => GNAT.Sockets.Inet_Addr (Host), Port => Port);
                  when Listen =>
                     --  Optionally use bind address for listener
                     Address :=
                       GNAT.Sockets.Network_Socket_Address
                         (Addr =>
                            GNAT.Sockets.Inet_Addr
                              ((if Has_Key ("bind") then Get_Value ("bind")
                                else Host)),
                          Port => Port);
               end case;
            end;

         when Unixexec =>
            Family := Family_Unix;

            case Mode is
               when Connect =>
                  if not Has_Key ("path") then
                     raise Address_Error
                       with "Connectable transport 'unixexec' requires 'path'";
                  end if;

                  if Has_Key ("argv0") then
                     raise Transport_Error
                       with "Connectable transport 'unixexec' does not" &
                       " support overriding 'argv0'";
                  end if;

                  --  Create sockets and spawn subprocess
                  declare
                     Client, Server : Socket_Type;
                     Discard        : GNATCOLL.OS.Process.Process_Handle;
                     Arguments      : GNATCOLL.OS.Process.Argument_List;
                     Argument_Index : Positive := 1;

                     function Convert is new Ada.Unchecked_Conversion
                       (Integer, GNATCOLL.OS.FS.File_Descriptor);
                  begin
                     Create_Socket_Pair (Client, Server, Family_Unix);

                     --  Add all arguments
                     Arguments.Append (Get_Value ("path"));
                     Add_Arguments :
                     loop
                        declare
                           Argument_Name : constant String :=
                             "argv" &
                             Ada.Strings.Fixed.Trim
                               (Source => Argument_Index'Image,
                                Side   => Ada.Strings.Left);
                        begin
                           if not Has_Key (Argument_Name) then
                              exit Add_Arguments;
                           else
                              Arguments.Append (Get_Value (Argument_Name));
                              Argument_Index := Argument_Index + 1;
                           end if;
                        end;
                     end loop Add_Arguments;

                     --  Start process
                     Discard :=
                       GNATCOLL.OS.Process.Start
                         (Args   => Arguments,
                          Stdin  => Convert (GNAT.Sockets.To_C (Server)),
                          Stdout => Convert (GNAT.Sockets.To_C (Server)));

                     return Client;
                  end;

               when Listen =>
                  raise Address_Error
                    with "Transport 'unixexec' not listenable";
            end case;

         when Autolaunch =>
            declare
               Info_File : Unbounded_String;
            begin
               --  Try using the environment variable DBUS_SESSION_BUS_ADDRESS
               if Ada.Environment_Variables.Exists ("DBUS_SESSION_BUS_ADDRESS")
               then
                  return
                    Try_Address
                      (Ada.Environment_Variables.Value
                         ("DBUS_SESSION_BUS_ADDRESS"));
               end if;

               --  If it doesn't exist, load from session file
               --  in home directory.

               --  Pick a directory to store instance info
               if Ada.Environment_Variables.Exists ("HOME") then
                  Info_File :=
                    To_Unbounded_String
                      (Ada.Environment_Variables.Value ("HOME"));
               elsif Ada.Environment_Variables.Exists ("LOCALAPPDATA") then
                  Info_File :=
                    To_Unbounded_String
                      (Ada.Environment_Variables.Value ("LOCALAPPDATA"));
               else
                  raise Transport_Error
                    with "Transport 'autolaunch' could not" &
                    " locate instance info";
               end if;

               Append (Info_File, "/.dbus/session-bus/");
               Append
                 (Info_File, To_Hex (String (D_Bus.Platform.Get_Machine_ID)));

               --  Append display name if we can locate it
               --  TODO move elsewhere?
               if Ada.Environment_Variables.Exists ("DISPLAY") then
                  Append (Info_File, "-");
                  declare
                     Full_Display : constant String  :=
                       Ada.Environment_Variables.Value ("DISPLAY");
                     Colon        : constant Natural :=
                       Ada.Strings.Fixed.Index (Full_Display, ":");
                     Cut_Display  :
                       String renames
                       Full_Display (Colon + 1 .. Full_Display'Last);
                     Dot          : Natural          :=
                       Ada.Strings.Fixed.Index (Cut_Display, ".");
                  begin
                     if Dot = 0 then
                        Dot := Cut_Display'Last + 1;
                     end if;
                     Append (Info_File, Cut_Display (1 .. Dot - 1));
                  end;
               end if;
               --  Note: No error if we can't determine display, it just
               --  means this will be a user-wide address.

               Ada.Text_IO.Put_Line
                 ("TODO debug use info file " & To_String (Info_File));

               --  Check whether the file and/or directory exist
               if not Ada.Directories.Exists (To_String (Info_File)) then
                  case Mode is
                     when Connect =>
                        raise Transport_Error
                          with "Connectable transport 'autolaunch' could not" &
                          " locate instance info";

                     when Listen =>
                        Ada.Directories.Create_Directory
                          (Ada.Directories.Containing_Directory
                             (To_String (Info_File)));
                  end case;
               end if;

               case Mode is
                  when Connect =>
                     declare
                        package Declaration_Maps is new Ada.Containers
                          .Indefinite_Hashed_Maps
                          (String, String, Ada.Strings.Hash, "=");

                        function Read_Info_File
                          (Path : String) return Declaration_Maps.Map;
                        function Read_Info_File
                          (Path : String) return Declaration_Maps.Map
                        is
                           use Ada.Text_IO;

                           File   : File_Type;
                           Result : Declaration_Maps.Map;
                        begin
                           Open (File, In_File, Path);

                           while not End_Of_File (File) loop
                              declare
                                 Line : constant String := Get_Line (File);
                                 Key_Name_End : Natural;
                              begin
                                 if Line (Line'First) /= '#' then
                                    Key_Name_End :=
                                      Ada.Strings.Fixed.Index (Line, "=");

                                    --  Valid placement check
                                    if Key_Name_End = 0 or
                                      Key_Name_End = Line'Last
                                    then
                                       pragma Style_Checks ("-M");
                                       raise Transport_Error
                                         with "Connectable transport 'autolaunch': unterminated session file line";
                                       pragma Style_Checks (On);
                                    end if;

                                    --  Insert key and value
                                    --  No preprocessing is recommended in the
                                    --  specification.
                                    Result.Insert
                                      (Key      =>
                                         Line (Line'First .. Key_Name_End - 1),
                                       New_Item =>
                                         Line (Key_Name_End + 1 .. Line'Last));
                                 end if;
                              end;
                           end loop;

                           Close (File);
                           return Result;
                        end Read_Info_File;

                        Declarations : constant Declaration_Maps.Map :=
                          Read_Info_File (To_String (Info_File));
                     begin
                        if not Declarations.Contains
                            ("DBUS_SESSION_BUS_ADDRESS") or
                          not Declarations.Contains ("DBUS_SESSION_BUS_PID")
                        then
                           raise Transport_Error
                             with "Connectable transport 'autolaunch'" &
                             " lacking required variables in session file";
                        end if;

                        --  Relaunch if process not running
                        if not D_Bus.Platform.Is_Running
                            (GNATCOLL.OS.Process.Process_Handle'Value
                               (Declarations.Element ("DBUS_SESSION_BUS_PID")))
                        then
                           raise Transport_Error
                             with "TODO add restart impl when not running";
                        end if;

                        --  Create socket
                        Address :=
                          Unix_Socket_Address
                            (Declarations.Element
                               ("DBUS_SESSION_BUS_ADDRESS"));
                        goto Autolaunch_Complete;
                     end;

                  when Listen =>
                     declare
                        use Ada.Text_IO;
                        File        : File_Type;
                        Socket_Path : constant String :=
                          Ada.Directories.Containing_Directory
                            (To_String (Info_File)) &
                          Random_Filename & ".sock";

                     begin
                        --  Create info file
                        Ada.Text_IO.Create
                          (File, Out_File, To_String (Info_File));
                        Put_Line
                          (File,
                           "DBUS_SESSION_BUS_ADDRESS=unix:path=" &
                           Encode_Server_Address (Socket_Path));
                        Put_Line
                          (File,
                           "DBUS_SESSION_BUS_PID=" &
                           Ada.Strings.Fixed.Trim
                             (Source =>
                                GNAT.OS_Lib.Pid_To_Integer
                                  (GNAT.OS_Lib.Current_Process_Id)'
                                  Image,
                              Side   => Ada.Strings.Left));
                        Ada.Text_IO.Close (File);

                        --  Create socket
                        --  TODO solve issue if name already exists
                        --  TODO also clean up after terminate (elsewhere)
                        Address := Unix_Socket_Address (Socket_Path);
                        goto Autolaunch_Complete;
                     end;
               end case;

               <<Autolaunch_Complete>>
            end;
      end case;

      --  Make a socket and return
      --  TODO handle connect failure
      declare
         Socket : Socket_Type;
      begin
         Create_Socket (Socket, Family);

         case Mode is
            when Connect =>
               Connect_Socket (Socket, Address);
            when Listen =>
               Listen_Socket (Socket);
               Bind_Socket (Socket, Address);
         end case;

         return Socket;
      end;
   exception
      --  Avoid propagating an exception (is slow)
      --  Instead print message and return null socket
      when X : Transport_Error =>
         declare
            use Ada.Text_IO;
            use Ada.Exceptions;
         begin
            Put_Line
              (Standard_Error,
               "[D-Bus][Connection] " & Addr & " " & Exception_Message (X));
            return GNAT.Sockets.No_Socket;
         end;
   end Try_Address;

   Group_Start : Positive := Addr'First;
   Group_End   : Natural;
   Temp        : GNAT.Sockets.Socket_Type;
   Result      : Socket_Set_Type;
begin
   --  Note: a group is at least two characters (protocol name and ':')
   while Group_Start < Addr'Last loop
      --  Determine the end of the current group
      Group_End := Ada.Strings.Fixed.Index (Addr, ";", Group_Start);
      if Group_End = 0 then
         Group_End := Addr'Last;
      end if;

      --  Try parsing one group
      Temp := Try_Address (Addr (Group_Start .. Group_End));
      if Temp /= GNAT.Sockets.No_Socket then
         GNAT.Sockets.Set (Result, Temp);
      end if;

      --  Update the start index
      Group_Start := Group_End + 1;
   end loop;

   --  TODO we need to clean up sockets when done
   --  not actually here though, in consumer code

   if GNAT.Sockets.Is_Empty (Result) then
      raise Transport_Error
        with "The provided address '" & Addr & "' could not be parsed.";
   end if;

   return R : Socket_Set_Type do
      GNAT.Sockets.Copy (Result, R);
   end return;
end D_Bus.Connection.Parse_Address;
