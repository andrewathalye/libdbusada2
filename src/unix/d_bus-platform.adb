pragma Ada_2012;

with Ada.Directories;
with Ada.Text_IO;

with Interfaces.C;

with D_Bus.Encodings;
with Interfaces.C.Strings;

package body D_Bus.Platform is
   -----------------
   -- Subprograms --
   -----------------
   function Read_All_File (Name : String) return String;
   function Read_All_File (Name : String) return String is
      use Ada.Text_IO;
      File : File_Type;
   begin
      Open (File, In_File, Name);

      return Res : constant String := Get_Line (File) do
         Close (File);
      end return;
   end Read_All_File;

   --------------------
   -- Implementation --
   --------------------
   function Get_Machine_ID return D_Bus.Types.UUID is
   begin
      if Ada.Directories.Exists ("/var/lib/dbus/machine-id") then
         return
           D_Bus.Types.UUID
             (D_Bus.Encodings.From_Hex
                (Read_All_File ("/var/lib/dbus/machine-id")));
      elsif Ada.Directories.Exists ("/etc/machine-id") then
         return
           D_Bus.Types.UUID
             (D_Bus.Encodings.From_Hex (Read_All_File ("/etc/machine-id")));
      else
         raise Program_Error with "Unable to determine machine id";
      end if;
   end Get_Machine_ID;

   function Get_User_ID return String is
      function Get_User_ID_C return Interfaces.C.Strings.chars_ptr with
        Import => True, Convention => C;
      Ptr : Interfaces.C.Strings.chars_ptr;
   begin
      Ptr := Get_User_ID_C;

      return Result : constant String := Interfaces.C.Strings.Value (Ptr) do
         Interfaces.C.Strings.Free (Ptr);
      end return;
   end Get_User_ID;

   function Is_Running
     (Handle : GNATCOLL.OS.Process.Process_Handle) return Boolean
   is
      function Is_Running_C
        (Handle : GNATCOLL.OS.Process.Process_Handle)
         return Interfaces.C.C_bool with
        Import => True, Convention => C;
   begin
      return Boolean (Is_Running_C (Handle));
   end Is_Running;

   Max_Possible_FDs : Integer with
     Import => True, Convention => C;

   function Read_FDs (Socket : GNAT.Sockets.Socket_Type) return FD_Array is
      type Result_Type is (Destructive, Transient, Success) with
        Convention => C;

      Buf      : aliased FD_Array (1 .. Max_Possible_FDs);
      FD_Count : Integer;

      function Read_FDs_C
        (Socket   :     Integer; FDs : access GNAT.OS_Lib.File_Descriptor;
         FD_Count : out Integer) return Result_Type with
        Import => True, Convention => C;
   begin
      --  TODO what happens if fds length is zero?
      case Read_FDs_C
          (Socket => GNAT.Sockets.To_C (Socket), FDs => Buf (Buf'First)'Access,
           FD_Count => FD_Count) is
         when Destructive =>
            raise File_Descriptor_Destructive_Error;
         when Transient =>
            raise File_Descriptor_Error;
         when Success =>
            return Buf (1 .. FD_Count);
      end case;
   end Read_FDs;

   procedure Write_FDs (Socket : GNAT.Sockets.Socket_Type; FDs : FD_Array) is
      use type Interfaces.C.C_bool;

      function Write_FDs_C
        (Socket   : Integer; FDs : access constant GNAT.OS_Lib.File_Descriptor;
         FD_Count : Integer) return Interfaces.C.C_bool with
        Import => True, Convention => C;
   begin
      --  TODO what happens if fds length is zero?
      if not Write_FDs_C
          (GNAT.Sockets.To_C (Socket), FDs (FDs'First)'Access, FDs'Length)
      then
         raise File_Descriptor_Error;
      end if;
   end Write_FDs;

   function FD_Transfer_Support (S : GNAT.Sockets.Socket_Type) return Boolean
   is
      use type GNAT.Sockets.Family_Type;
   begin
      return
        GNAT.Sockets.Get_Socket_Name (S).Family = GNAT.Sockets.Family_Unix;
   end FD_Transfer_Support;

   function Read_Credentials (S : GNAT.Sockets.Socket_Type) return String is
      use type Interfaces.C.Strings.chars_ptr;

      function Read_Credentials_C
        (S : Integer) return Interfaces.C.Strings.chars_ptr with
        Import => True, Convention => C;

      Ptr : Interfaces.C.Strings.chars_ptr;
   begin
      Ptr := Read_Credentials_C (GNAT.Sockets.To_C (S));

      if Ptr = Interfaces.C.Strings.Null_Ptr then
         raise Credentials_Error;
      end if;

      return Result : constant String := Interfaces.C.Strings.Value (Ptr) do
         Interfaces.C.Strings.Free (Ptr);
      end return;
   end Read_Credentials;

   procedure Write_Credentials (S : GNAT.Sockets.Socket_Type) is
      use type Interfaces.C.C_bool;

      function Write_Credentials_C
        (S : Integer) return Interfaces.C.C_bool with
        Import => True, Convention => C;
   begin
      if not Write_Credentials_C (GNAT.Sockets.To_C (S)) then
         raise Credentials_Error;
      end if;
   end Write_Credentials;
end D_Bus.Platform;
