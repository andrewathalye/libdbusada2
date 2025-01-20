pragma Ada_2022;

with Ada.Text_IO; use Ada.Text_IO;

with D_Bus.Types.Basic;
   use D_Bus.Types.Basic;
with D_Bus.Types.Containers;
use type D_Bus.Types.Containers.Variant;
with D_Bus.Types.Dispatching_Read;
with D_Bus.Connection;

procedure Tests is
   Arr_V : D_Bus.Types.Containers.D_Array (D_Bus.Types.Intern ("v"));

   Dict_sav : D_Bus.Types.Containers.Dict ('s', D_Bus.Types.Intern ("av"));

   Struct_esav : D_Bus.Types.Containers.Struct :=
     D_Bus.Types.Containers.Empty ("a{sav}");

   Stream : aliased D_Bus.Connection.Alignable_Stream;
begin
   Arr_V.Append (+Byte'(1));

   Dict_sav.Insert (D_String'("Hello"), Arr_V);

   Struct_esav.Set (1, Dict_sav);

   Put_Line (Struct_esav.Image);

   D_Bus.Connection.Open_Test_Stream (Stream);
   D_Bus.Types.Containers.Struct'Write (Stream'Access, Struct_esav);
   Put_Line (D_Bus.Types.Dispatching_Read (Stream'Access, "(a{sav})").Image);
   D_Bus.Connection.Close_Test_Stream (Stream);

   --  (a{sav})
   --  ([{Hello.s => [v'1.y]}])
   --  [struct]
   --  ALIGN8 <> 0
   --     [array]
   --     ALIGN4 <> 0
   --     Length 4
   --        [dict]
   --        ALIGN8 <> 4
   --           [string]
   --           ALIGN4 <> 0
   --           Length .5 4
   --           Data .Hello 5
   --           Null_Byte 1
   --
   --           [array] 4 / 6
   --           ALIGN4 <> 2
   --           Length .4 4
   --              [variant] 4
   --                 [signature]
   --                 Length .1 1
   --                 Data .y 1
   --                 Null_Byte 1
   --
   --                 [byte]
   --                 data .1 1
end Tests;
