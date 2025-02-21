pragma Ada_2012;

with Ada.Text_IO; use Ada.Text_IO;
with Test_Defs; use Test_Defs;

procedure Tests is
   package Generic_Instance is new Generic_Pkg;
   Works : Generic_Instance.Works;
   Crashes : Generic_Instance.Crashes;
   Also_Crashes : Generic_Instance.Also_Crashes;
begin
   Put_Line (Works.PBT);
   Put_Line (Derived_Type'Class (Works).PBT);
   Put_Line (Base_Type'Class (Works).PBT);

   Put_Line (Crashes.PBT);
   Put_Line (Derived_Type'Class (Crashes).PBT);
   --  Put_Line (Base_Type'Class (Crashes).PBT);
   --  ^^ CRASH HERE

   Put_Line (Also_Crashes.PBT);
   Put_Line (Derived_Type'Class (Also_Crashes).PBT);
   --  Put_Line (Base_Type'Class (Also_Crashes).PBT);
   --  ^^ CRASH HERE
end Tests;
