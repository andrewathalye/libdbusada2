pragma Ada_2012;

with Ada.Containers.Indefinite_Hashed_Maps;
with Ada.Directories;
with Ada.Environment_Variables;
with Ada.Strings.Fixed;
with Ada.Strings.Hash;
with Ada.Strings.Maps;
with Ada.Strings.Unbounded;
with Ada.Text_IO;
with Ada.Unchecked_Conversion;

with GNAT.OS_Lib;

with GNATCOLL.OS.FS;
with GNATCOLL.OS.Process;

with D_Bus.Platform;
with D_Bus.Encodings;
with D_Bus.Logging; use D_Bus.Logging;

function D_Bus.Connection.Parse_Address
  (Mode : Mode_Type; Addr : D_Bus.Types.Extra.Server_Address)
   return GNAT.Sockets.Socket_Set_Type
is
   use GNAT.Sockets;

   type Server_Transport is
     (Unix, Launchd, Systemd, Tcp, Unixexec, Autolaunch);
   --  These are the server transport protocols that we support.
   --  Nonce-Tcp is not supported.

   function Try_Address
     (Addr : D_Bus.Types.Extra.Server_Address) return GNAT.Sockets.Socket_Type;
   --  Try to convert an address specification (single address) to a socket

   function Try_Address
     (Addr : D_Bus.Types.Extra.Server_Address) return GNAT.Sockets.Socket_Type
   is
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
         Equal_Pos      : Natural;
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
                 Ada.Strings.Fixed.Index (Transport_Props, ",", Equal_Pos) + 1;
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
            V_End := Transport_Props'Last + 1;
         end if;

         return Decode_Server_Address (Transport_Props (V_Start .. V_End - 1));
      end Get_Value;

      function Has_Key (Key : String) return Boolean is
        (Key_Value_Start (Key) /= 0);
      --  Return True if Transport_Props contains the given key,
      --  and False if it is not present.

      function Random_Filename return String;
      function Random_Filename return String is
        (D_Bus.Encodings.To_Hex (String (New_UUID)));

      ----------
      -- Data --
      ----------
      Transport : Server_Transport;
      Family    : Family_Type;
      Address   : Sock_Addr_Type;
      --  Note Compiler will warn if it is possible for these to be
      --  uninitialised.
   begin
      Log (Info, "Parse address " & Addr);

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
                  --  TODO needs testing
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
                        Args.Append (Env);

                        Result :=
                          GNATCOLL.OS.Process.Run
                            (Args => Args, Status => Status);

                        --  Check command success
                        if Status /= 0 then
                           raise Transport_Error
                             with "Connectable transport 'launchd' failed" &
                             " to get socket path.";
                        end if;

                        --  Data as read in contains line terminator
                        declare
                           CR_LF_Space :
                             constant Ada.Strings.Maps.Character_Set :=
                             Ada.Strings.Maps.To_Set
                               (ASCII.CR & ASCII.LF & ' ');
                        begin
                           Address :=
                             Unix_Socket_Address
                               (Ada.Strings.Fixed.Trim
                                  (To_String (Result), CR_LF_Space,
                                   CR_LF_Space));
                        end;
                     exception
                        when GNATCOLL.OS.OS_Error =>
                           raise Transport_Error
                             with "Connectable transport 'launchd' could" &
                             " not call launchtl";
                     end;

                     --  TODO needs testing
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

            --  TODO needs testing!
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

            --  Look up host, then Connect / Listen
            --  TODO see if this works with '*' as per spec
            declare
               Pref_Family : constant Family_Type :=
                 (if Has_Key ("family") then
                    (if Get_Value ("family") = "ipv4" then Family_Inet
                     elsif Get_Value ("family") = "ipv6" then Family_Inet6
                     else raise Transport_Error
                         with "Transport 'tcp' does not support this family")
                  else Family_Unspec);

               AIA : constant Address_Info_Array :=
                 Get_Address_Info
                   (Host    =>
                      (if Has_Key ("bind") and Mode = Listen then
                         Get_Value ("bind")
                       else Get_Value ("host")),
                    Service => Get_Value ("port"), Family => Pref_Family);
            begin
               if AIA'Length = 0 then
                  raise Transport_Error
                    with "Connectable transport 'tcp' found no addresses.";
               end if;

               Address := AIA (AIA'First).Addr;
               Family  := Address.Family;
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
                     begin
                        Discard :=
                          GNATCOLL.OS.Process.Start
                            (Args   => Arguments,
                             Stdin  => Convert (GNAT.Sockets.To_C (Server)),
                             Stdout => Convert (GNAT.Sockets.To_C (Server)));
                     exception
                        when GNATCOLL.OS.OS_Error =>
                           raise Transport_Error
                             with "Connectable transport 'unixexec' failed" &
                             " to launch process.";
                     end;

                     --  We don't need the server socket
                     GNAT.Sockets.Close_Socket (Server);

                     return Client;
                  end;

                  --  TODO needs testing
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
                  declare
                     Env_Addr : constant String :=
                       Ada.Environment_Variables.Value
                         ("DBUS_SESSION_BUS_ADDRESS");
                  begin
                     if Env_Addr in D_Bus.Types.Extra.Server_Address then
                        return
                          Try_Address
                            (Ada.Environment_Variables.Value
                               ("DBUS_SESSION_BUS_ADDRESS"));
                     else
                        pragma Style_Checks ("-M");
                        Log
                          (Warning,
                           "Found environment variable DBUS_SESSION_BUS_ADDRESS with invalid contents '" &
                           Env_Addr & "'");
                        pragma Style_Checks (On);
                     end if;
                  end;
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

               --  Note:
               --  Append display name if we can locate it.
               --  An invalid format will not cause a crash.
               --
               --  Failure to determine display means that the file we use will be
               --  valid user-wide, rather than just for the current display.
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
                     Append
                       (Info_File, Cut_Display (Cut_Display'First .. Dot - 1));
                  end;
               end if;

               --  Check whether the file and/or directory exist
               Log
                 (Info, "Load autolaunch data from " & To_String (Info_File));
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

                           Single_Quotes :
                             constant Ada.Strings.Maps.Character_Set :=
                             Ada.Strings.Maps.To_Set (''');
                           File          : File_Type;
                           Result        : Declaration_Maps.Map;
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
                                    --  Note: although not specified, in practice
                                    --  we need to remove single quotes from
                                    --  the beginning and end of the value.
                                    Result.Insert
                                      (Key      =>
                                         Line (Line'First .. Key_Name_End - 1),
                                       New_Item =>
                                         Ada.Strings.Fixed.Trim
                                           (Source =>
                                              Line
                                                (Key_Name_End + 1 ..
                                                     Line'Last),
                                            Left   => Single_Quotes,
                                            Right  => Single_Quotes));
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

                        declare
                           Found_Addr : constant String :=
                             Declarations.Element ("DBUS_SESSION_BUS_ADDRESS");
                        begin
                           if Found_Addr not in
                               D_Bus.Types.Extra.Server_Address
                           then
                              raise Transport_Error
                                with "Connectable transport 'autolaunch' failed to connect to address '" &
                                Found_Addr & "'";
                           end if;

                           return Try_Address (Found_Addr);
                        end;
                     end;
                  --  TODO needs testing
                  when Listen =>
                     <<Try_Again>>
                     declare
                        use Ada.Text_IO;
                        File        : File_Type;
                        Socket_Path : constant String :=
                          Ada.Directories.Containing_Directory
                            (To_String (Info_File)) &
                          Random_Filename & ".sock";

                     begin
                        --  Try again if the socket path already exists
                        if Ada.Directories.Exists (Socket_Path) then
                           goto Try_Again;
                        end if;

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

                        --  Set socket address
                        Address := Unix_Socket_Address (Socket_Path);
                     end;
               end case;
            end;
      end case;

      --  Make a socket and return
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
      when X : Transport_Error | Socket_Error =>
         Log (Error, X);
         return GNAT.Sockets.No_Socket;
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

   if GNAT.Sockets.Is_Empty (Result) then
      raise Transport_Error
        with "Unable to connect to any addresses in '" & Addr & "'";
   end if;

   return R : Socket_Set_Type do
      GNAT.Sockets.Copy (Result, R);
   end return;
end D_Bus.Connection.Parse_Address;
