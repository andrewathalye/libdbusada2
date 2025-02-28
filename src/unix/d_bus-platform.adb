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

   procedure Read_FD
     (Socket : GNAT.Sockets.Socket_Type; FD : out GNAT.OS_Lib.File_Descriptor)
   is
      pragma Unreferenced (Socket);
      pragma Unused (FD);
   begin
      raise Program_Error;
   end Read_FD;

   procedure Write_FD
     (Socket : GNAT.Sockets.Socket_Type; FD : GNAT.OS_Lib.File_Descriptor)
   is
      pragma Unreferenced (Socket);
      pragma Unreferenced (FD);
   begin
      raise Program_Error;
   end Write_FD;

   function FD_Transfer_Support (S : GNAT.Sockets.Socket_Type) return Boolean
   is
      use type GNAT.Sockets.Family_Type;
   begin
      return
        GNAT.Sockets.Get_Socket_Name (S).Family = GNAT.Sockets.Family_Unix;
   end FD_Transfer_Support;

   function Read_Credentials (S : GNAT.Sockets.Socket_Type) return String
   is
      use type Interfaces.C.Strings.chars_ptr;

      function Read_Credentials_C
        (S : Integer) return Interfaces.C.Strings.chars_ptr
        with Import => True, Convention => C;

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

      function Write_Credentials_C (S : Integer) return Interfaces.C.C_bool
      with Import => True, Convention => C;
   begin
      if not Write_Credentials_C (GNAT.Sockets.To_C (S)) then
         raise Credentials_Error;
      end if;
   end Write_Credentials;
end D_Bus.Platform;
