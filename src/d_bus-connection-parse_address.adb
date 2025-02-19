pragma Ada_2012;

with Ada.Directories;
with Ada.Environment_Variables;
with Ada.Exceptions;
with Ada.Strings.Fixed;
with Ada.Strings.Unbounded;
with Ada.Text_IO;

with GNAT.OS_Lib;

with GNATCOLL.OS.FS;
with GNATCOLL.OS.Process;

with D_Bus.Standard_Interfaces;

pragma Style_Checks (Off);
function D_Bus.Connection.Parse_Address
     (Mode : Mode_Type; Addr : Server_Address) return GNAT.Sockets.Socket_Set_Type
is
   use GNAT.Sockets;

   type Server_Transport is (Unix, Launchd, Systemd, Tcp, Unixexec, Autolaunch);
   --  These are the server transport protocols that we support.
   --  Nonce-Tcp is not supported.

   type Nullable_Socket_Type (Has_Socket : Boolean) is record
      case Has_Socket is
         when True => 
            Socket : Socket_Type;
         when False =>
            null;
      end case;
   end record;

   --  TODO more checks, check whether the address exists if unix
   function Try_Address
     (Addr : String; Last : out Positive) return Nullable_Socket_Type;
   function Try_Address
     (Addr : String; Last : out Positive) return Nullable_Socket_Type
   is
      use Ada.Strings.Unbounded;

      function Get_Value (Key : String) return String;
      function Has_Key (Key : String) return Boolean;

      Transport_Name : constant String := Addr (Addr'First .. Ada.Strings.Fixed.Index (Addr, ":"));
      Transport : Server_Transport;
      Socket : Socket_Type;
   begin
      Last := Ada.Strings.Fixed.Index (Addr, ";") + 1;

      --  Try to parse the transport
      begin
         Transport := Server_Transport'Value (Transport_Name);
      exception
         when Constraint_Error =>
            raise Transport_Error with "Transport '" & Transport_Name & "' is not recognised.";
      end;

      case Transport is
         when Unix =>
            if Has_Key ("abstract") then
               --  This is valid syntax, just not supported
               raise Transport_Error with "Transport 'Unix' does not support abstract sockets.";
            end if;

            case Mode_Type is
               when Connect =>
                  if not Has_Key ("path") then
                     raise Address_Error with "Connectable transport 'unix' requires option 'path'.";
                  end if;

                  GNAT.Sockets.Create_Socket (Socket, Family_Unix); 
                  Address := Unix_Socket_Address (Get_Value ("path"));
                  --  TODO timeout
                  GNAT.Sockets.Connect_Socket (Socket, Address);
               when Listen =>
                  declare
                     Address : Sock_Addr_Type;
                  begin
                     if Has_Key ("path") then
                        Address := Unix_Socket_Address (Get_Value ("path"));
                     elsif Has_Key ("dir") then
                        Address := Unix_Socket_Address (Get_Value ("dir") & "/" & Random_Name);
                     elsif Has_Key ("tmpdir") then
                        Address := Unix_Socket_Address (Get_Value ("tmpdir") & "/" & Random_Name);
                     elsif Has_Key ("runtime") then
                        if Get_Value ("runtime") /= "yes" then
                           raise Address_Error with "Listenable transport 'unix' takes option 'runtime=yes'";
                        end if;

                        if not Ada.Environment_Variables.Exists ("XDG_RUNTIME_DIR") then
                           raise Transport_Error with "XDG_RUNTIME_DIR not defined and 'runtime=yes' requested.";
                        end if;

                        Address := Unix_Socket_Address (Ada.Environment_Variables.Value ("XDG_RUNTIME_DIR") & "/" & Random_Name);
                     else
                        raise Address_Error with "Listenable transport 'unix' requires options";
                     end if;

                     GNAT.Sockets.Create_Socket (Socket, Family_Unix);
                     GNAT.Sockets.Listen_Socket;
                     GNAT.Sockets.Bind_Socket (Socket, Address);
                  end;
            end case;
         when Launchd =>
            if not Has_Key ("env") then
               raise Address_Error with "Transport 'launchd' requires option 'env'.";
            end if;

            --  Note:
            --  "env" - per documentation, contains the name of an
            --   environment variable that then contains the real
            --   socket path.
            declare
               Env : constant String := Get_Value ("env");
            begin
               case Mode_Type is
                  when Connect =>
                     --  Retrieve Socket using "launchctl getenv <...>"
                     declare
                        Status : Integer;
                        Read_Pipe, Write_Pipe : GNATCOLL.OS.FS.File_Descriptor;
                        Result : Socket_Addr_Type;
                     begin
                        GNATCOLL.OS.FS.Open_Pipe (Read_Pipe, Write_Pipe);

                        --  Make sure write pipe propagates
                        GNATCOLL.OS.FS.Set_Close_On_Exec (Write_Pipe, False);

                        --  Blocking `Run` until error or completion
                        Status := GNATCOLL.OS.Process.Run
                          (Args => ("launchctl", "getenv", Env),
                           Stdout => Write_Pipe);

                        --  TODO is this always 0 on success?
                        if Status /= 0 then
                           GNATCOLL.OS.FS.Close (Read_Pipe);
                           GNATCOLL.OS.FS.Close (Write_Pipe);
                           raise Transport_Error with "Connectable transport 'launchd' failed to get socket path.";
                        end if;

                        --  Fetch and stash result
                        --  Note: This can FAIL if the result is too large
                        --  to store on the stack. This isn’t a security
                        --  risk, is it? TODO
                        Result := Unix_Socket_Address (GNATCOLL.OS.FS.Read (Read_Pipe));

                        --  Clean up
                        GNATCOLL.OS.FS.Close (Read_Pipe);
                        GNATCOLL.OS.FS.Close (Write_Pipe);

                        --  Remove white space and make socket
                        GNAT.Sockets.Create_Socket (Socket, Family_Unix);
                        GNAT.Sockets.Connect_Socket (Socket, Unix_Socket_Address (Ada.Strings.Fixed.Trim (Result)));
                     end;
                  when Listen =>
                     if not Ada.Environment_Variables.Exists (Env) then
                        raise Transport_Error with "Listenable transport 'launchd' must be launched via launchd.";
                     end if;

                     GNAT.Sockets.Create_Socket (Socket, Family_Unix);
                     GNAT.Sockets.Listen_Socket (Socket);
                     GNAT.Sockets.Bind_Socket (Socket, Unix_Socket_Address (Ada.Environment_Variables.Value (Env)));
               end case;
            end;
         when Systemd =>
            case Mode is
               when Connect =>
                  raise Transport_Error with "Transport 'systemd' is not connectable.";
               when Listen =>
                  if not Ada.Environment_Variables.Exists ("LISTEN_FDS") then
                     raise Transport_Error with "Listenable transport 'systemd' must be launched via systemd.";
                  end if;

                  if Ada.Environment_Variables.Value ("LISTEN_FDS") /= "1" then
                     raise Transport_Error with "Listenable transport 'systemd' requires one fd.";
                  end if;

                  --  The first FD systemd uses is '3'
                  Socket := GNAT.Sockets.To_Ada (3);
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
                  GNAT.Sockets.Create_Socket (Socket, Family_Inet);
               elsif Get_Value ("family") = "ipv6" then
                  GNAT.Sockets.Create_Socket (Socket, Family_Inet6);
               else
                  raise Address_Error with "Transport 'tcp' does not accept the specified 'family'";
               end if;
            else
               --  One last attempt to identify IPv6 addresses
               if GNAT.Sockets.Is_IPv6_Address (Get_Value ("host")) then
                  GNAT.Sockets.Create_Socket (Socket, Family_Inet6);
               else
                  --  No family specified, IPv4 will have to do
                  GNAT.Sockets.Create_Socket (Socket, Family_Inet);
               end if;
            end if;

            --  Connect / Listen / Accept
            declare
               Host : constant String := Get_Value ("host");
               Port : constant Port_Type := Port_Type'Value (Get_Value ("port"));
            begin
               case Mode is
                  when Connect =>
                     GNAT.Sockets.Connect_Socket
                       (Socket => Socket,
                        Server => GNAT.Sockets.Network_Socket_Address
                          (Addr => GNAT.Sockets.Inet_Addr (Host),
                           Port => Port));
                  when Listen =>   
                     GNAT.Sockets.Listen_Socket (Socket);

                     --  Optionally use bind address for listener
                     GNAT.Sockets.Bind_Socket
                       (Socket => Socket,
                        Address => GNAT.Sockets.Network_Socket_Address
                          (Addr => GNAT.Sockets.Inet_Addr
                             ((if Has_Key ("bind") then Get_Value ("bind") else Host)),
                           Port => Port));
               end case;
            end;
         when Unixexec =>
            case Mode is
               when Connect =>
                  if not Has_Key ("path") then
                     raise Address_Error with "Connectable transport 'unixexec' requires 'path'";
                  end if;

                  if Has_Key ("argv0") then
                     raise Transport_Error with "Connectable transport 'unixexec' does not support 'argv0'";
                  end if;

                  --  Create sockets and spawn subprocess
                  declare
                     Client, Server : Socket_Type;
                     Arguments : GNATCOLL.OS.Process.Argument_List;
                     Argument_Index : Positive := 1;
                  begin
                     Create_Socket_Pair (Client, Server, Family_Unix);

                     --  Add all arguments
                     Arguments.Append (Get_Value ("path"));
                     Add_Arguments :
                     loop
                        declare
                           Argument_Name : constant String := "argv" & Ada.Strings.Fixed.Trim (Argument_Index'Image);
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
                     GNATCOLL.OS.Process.Start
                       (Args => Arguments,
                        Stdin => Convert (GNAT.Sockets.To_C (Server)),
                        Stdout => Convert (GNAT.Sockets.To_C (Server)));

                     Socket := Client;
                  end;
               when Listen =>
                  raise Address_Error with "Transport 'unixexec' not listenable";
            end case;
         when Autolaunch =>
            declare
               use Ada.Strings.Unbounded.Unbounded_String;
               Info_File : Unbounded_String;
            begin
               --  Try using the environment variable DBUS_SESSION_BUS_ADDRESS
               if Ada.Environment_Variables.Exists ("DBUS_SESSION_BUS_ADDRESS") then
                  declare
                     Discard : Natural;
                  begin
                     return Try_Address
                       (Ada.Environment_Variables.Value ("DBUS_SESSION_BUS_ADDRESS"),
                        Discard);
                  end;
                  goto Autolaunch_Complete;
               end if;

               --  If it doesn’t exist, we have to load instance data from
               --  a file in the home directory.

               --  Pick a directory to store instance info
               if Ada.Environment_Variables.Exists ("HOME") then
                  Info_File := To_Unbounded (Ada.Environment_Variables.Value ("HOME"));
               elsif Ada.Environment_Variables.Exists ("LOCALAPPDATA") then
                  Info_File := To_Unbounded (Ada.Environment_Variables.Value ("LOCALAPPDATA"));
               else
                  raise Transport_Error with "Transport 'autolaunch' could not locate instance info";
               end if;

               Append (Info_File, "/.dbus/session-bus/");
               Append (Info_File, D_Bus.Standard_Interfaces.org_freedesktop_DBus_Peer.GetMachineId);

               --  Append display name if we can locate it
               if Ada.Environment_Variables.Exists ("DISPLAY") then
                  Append (Info_File, "-");
                  declare
                     Full_Display : constant String := Ada.Environment_Variables.Value ("DISPLAY");
                     Colon : constant Natural := Ada.Strings.Fixed.Index (Full_Display, ":");
                     Cut_Display : String renames Full_Display (Colon + 1 .. Full_Display'Last);
                     Dot : Natural := Ada.Strings.Fixed.Index (Cut_Display, ".");
                  begin
                     if Dot = 0 then
                        Dot := Cut_Display'Last + 1;
                     end if;
                     Append (Info_File, Cut_Display (1 .. Dot - 1));
                  end;
               end if;
               --  Note: No error if we can’t, just means X11 isn’t
               --  running and this will be a user-wide address rather
               --  than session-wide.

               Ada.Text_IO.Put_Line ("TODO use info file " & To_String (Info_File));

               --  Check whether the file and/or directory exist
               if not Ada.Directories.Exists (To_String (Info_File)) then
                  case Mode is
                     when Connect =>
                        raise Transport_Error with "Connectable transport 'autolaunch' could not locate instance info";
                     when Listen =>
                        Ada.Directories.Create_Directory (Ada.Directories.Containing_Directory (To_String (Info_File)));
                  end case;
               end if;

               case Mode is
                  when Connect =>
                     <<Reread_Info_File>>
                     declare
                        Declarations : constant Declaration_List := Read_Info_File (Info_File);
                     begin
                        if not Declarations.Contains ("DBUS_SESSION_BUS_ADDRESS") then
                           raise Transport_Error with "TODO no address in file";
                        end if;

                        --  Relaunch if process not running
                        --  TODO can we do better?
                        if not Declarations.Contains ("DBUS_SESSION_BUS_PID") then
                           raise Transport_Error with "TODO no pid in file";
                        elsif not Is_Running (Declarations.Get ("DBUS_SESSION_BUS_PID")) then
                           Ada.Text_IO.Put_Line ("Session bus not running, restart TODO");
                           GNATCOLL.OS.Process.Run (("dbus_launch"));
                           goto Reread_Info_File;
                        end if;

                        --  Create socket
                        GNAT.Sockets.Create_Socket (Socket, Family_Unix);
                        GNAT.Sockets.Connect_Socket (Socket, Unix_Socket_Address (Declarations.Get ("DBUS_SESSION_BUS_PID")));
                        goto Autolaunch_Complete;
                     end;
                  when Listen =>
                     declare
                        use Ada.Text_IO;
                        File : File_Type;
                        Socket_Addr : constant String := Ada.Directories.Containing_Directory (To_String (Info_File)) & Random_Name & ".sock";
                     begin
                        --  Create info file
                        Ada.Text_IO.Create (File, Out_File, To_String (Info_File));
                        Put_Line (File, "DBUS_SESSION_BUS_ADDRESS=unix:path=" & Encode (Socket_Addr));
                        Put_Line (File, "DBUS_SESSION_BUS_PID=" & Ada.Strings.Fixed.Trim (GNAT.OS_Lib.Pid_To_Integer (GNAT.Os_Lib.Current_Process_Id)));
                        Ada.Text_IO.Close (File);

                        --  Create socket
                        GNAT.Sockets.Create_Socket (Socket, Family_Unix);
                        GNAT.Sockets.Listen_Socket;
                        GNAT.Sockets.Bind_Socket (Socket, Unix_Socket_Address (Socket_Addr));
                        goto Autolaunch_Complete;
                     end;
               end case;

               raise Program_Error;
               <<Autolaunch_Complete>>
            end;
      end case;

      return Nullable_Socket_Type'(True, Socket);
   exception
      when X : Transport_Error =>
         declare
            use Ada.Text_IO;
            use Ada.Exceptions;
         begin
            Put_Line (Standard_Error, "[D-Bus][Connection] " & Addr & " " & Exception_Message (X));
            return Nullable_Socket_Type'(False);
         end;
   end Try_Address;

   Address_Count : constant Positive := Ada.Strings.Fixed.Count (Addr, ";") + 1;
   Index : Positive := Addr'First;
   Temp : Nullable_Socket_Type;
   Result : Socket_Set_Type;
begin
   for I in Result'Range loop
      Temp := Try_Address (Addr (Index .. Addr'Last), Index);
      if Temp.Has_Socket then
         GNAT.Sockets.Set (Result, Temp.Socket);
      end if;
   end loop;

   --  TODO we need to clean up sockets when done

   if not GNAT.Sockets.Is_Empty (Result) then
      return Result;
   end if;

   return raise Transport_Error with "No addresses could be used in the specified mode.";
end D_Bus.Connection.Parse_Address;
pragma Style_Checks (On);
