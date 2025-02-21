pragma Ada_2012;

with Ada.Directories;
with Ada.Text_IO;

with Interfaces.C;

package body D_Bus.Platform is
   --------------
   -- Imported --
   --------------
   subtype pid_t is Interfaces.C.int;
   function getpgid (pid : pid_t) return pid_t
   with
      Import => True, Convention => C;

   -----------------
   -- Subprograms --
   -----------------
   function Read_All_File (Name : String) return String;
   function Read_All_File (Name : String) return String
   is
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
   function Get_Machine_ID return String is
   begin
      if Ada.Directories.Exists ("/var/lib/dbus/machine-id") then
         return Read_All_File ("/var/lib/dbus/machine-id");
      elsif Ada.Directories.Exists ("/etc/machine-id") then
         return Read_All_File ("/etc/machine-id");
      else
         raise Program_Error with "Unable to determine machine id";
      end if;
   end Get_Machine_ID;

   function Is_Running
     (Handle : GNATCOLL.OS.Process.Process_Handle) return Boolean
   is
      use type Interfaces.C.int;
   begin
      return getpgid (pid_t (Handle)) > 0;
   end Is_Running;

end D_Bus.Platform;
