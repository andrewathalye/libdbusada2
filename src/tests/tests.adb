pragma Ada_2022;

with Ada.Text_IO;

with D_Bus.Types.Basic;

procedure Tests is
   Y : D_Bus.Types.Basic.Byte;
begin
   Ada.Text_IO.Put_Line ("START");
   Ada.Text_IO.Put_Line (Y.Image);
   Ada.Text_IO.Put_Line (String (Y.Signature));
end Tests;
