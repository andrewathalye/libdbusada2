with Ada.Directories;
with Ada.Text_IO;

package body D_Bus.Platform_Dependent is
   function Get_Machine_ID return String is
   begin
      if Ada.Directories.Exists ("/var/lib/dbus/machine-id") then
            declare
               File : Ada.Text_IO.File_Type;
            begin
               Ada.Text_IO.Open (File => File, Name => "/var/lib/dbus/machine-id");
               return Result : String := Ada.Text_IO.Get_Line (File) do
                  Ada.Text_IO.Close (File);
               end return;
            end;
         elsif Ada.Directories.Exists ("/etc/machine-id") then.
            declare
               File : Ada.Text_IO.File_Type;
            begin
               Ada.Text_IO.Open (File => File, Name => "/etc/machine-id");
               return Result : String := Ada.Text_IO.Get_Line (File) do
                  Ada.Text_IO.Close (File);
               end return;
            end;
         else
            raise Program_Error with "No machine ID could be found.";
      end if;
   end Get_Machine_ID;
end D_Bus.Platform_Dependent;
