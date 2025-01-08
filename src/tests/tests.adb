pragma Ada_2022;
pragma Warnings (Off);

with Ada.Text_IO;
use Ada.Text_IO;

with Ada.Tags;

with D_Bus.Types.Basic;
with D_Bus.Types.Containers;

procedure Tests is
   Y : D_Bus.Types.Basic.Byte;
   package V_Arrays is new D_Bus.Types.Containers.Arrays
     (Inner_Signature => "v");
   package I64_ASV_Dicts is new D_Bus.Types.Containers.Dicts (
      Key_Type_Code => D_Bus.Types.Int64_CC,
      Value_Signature => "av");

   use type D_Bus.Types.Containers.Variant;
   use type D_Bus.Types.Basic.Int64;
   use type D_Bus.Types.Basic.D_String;


   Str : D_Bus.Types.Basic.D_String;
   Var : D_Bus.Types.Containers.Variant := +(+"");
   Arr : V_Arrays.D_Array;

   Num : D_Bus.Types.Basic.Int64;
   Dic : I64_ASV_Dicts.Dict;
begin
   Str := +"Hello";
   Var := +Str;
   Arr.Append (Var);
   Num := +203;
   Dic.Insert (Num, Arr);

   Put_Line (Dic.Image);
end Tests;
