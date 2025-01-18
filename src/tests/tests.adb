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

   package Arrs_V is new D_Bus.Types.Containers.Arrays ("v");
   Arr_V : Arrs_V.D_Array;

   package Dicts_sav is new D_Bus.Types.Containers.Dicts ('s', "av");
   Dict_sav : Dicts_sav.Dict;

   package Structs_esav is new D_Bus.Types.Containers.Structs ("a{sav}");
   Struct_esav : Structs_esav.Struct;
begin
   Arr_V.Append (Var);

   Dict_sav.Insert (Str, Arr_V);

   Struct_esav.Set (1, Dict_sav);

   Put_Line (Struct_esav.Image);
end Tests;
