pragma Ada_2022;

with Ada.Text_IO;

with D_Bus.Types;

procedure Tests is
   Y : D_Bus.Types.Byte;
begin
   Ada.Text_IO.Put_Line ("START");
   Ada.Text_IO.Put_Line (Y'Image);
end Tests;
