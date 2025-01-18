pragma Ada_2012;

with Ada.Text_IO; use Ada.Text_IO;

with D_Bus.Types.Basic;
use type D_Bus.Types.Basic.Byte;
use type D_Bus.Types.Basic.D_String;
with D_Bus.Types.Containers;
use type D_Bus.Types.Containers.Variant;

procedure Tests is
   Byte : constant D_Bus.Types.Basic.Byte         := +1;
   Var  : constant D_Bus.Types.Containers.Variant := +Byte;
   Str  : constant D_Bus.Types.Basic.D_String     := +"Hello";

   Arr_V : D_Bus.Types.Containers.D_Array (D_Bus.Types.Intern ("v"));

   Dict_sav : D_Bus.Types.Containers.Dict ('s', D_Bus.Types.Intern ("av"));

   Struct_esav : D_Bus.Types.Containers.Struct :=
     D_Bus.Types.Containers.Empty ("a{sav}");
begin
   Arr_V.Append (Var);

   Dict_sav.Insert (Str, Arr_V);

   Struct_esav.Set (1, Dict_sav);

   Put_Line (Struct_esav.Image);
end Tests;
