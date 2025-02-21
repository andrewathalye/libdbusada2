package Test_Defs is
   type Base_Type is interface;
   function PBT (O : Base_Type) return String is abstract;

   type Any_Interface is interface;
   type Derived_Type is interface
      and Any_Interface and Base_Type;

   generic
   package Generic_Pkg is
      type Works is new Base_Type and Derived_Type with null record;
      type Crashes is new Derived_Type and Base_Type with null record;
      type Also_Crashes is new Derived_Type with null record;

      overriding
      function PBT (O : Works) return String is ("PBT");

      overriding
      function PBT (O : Crashes) return String is ("PBT");

      overriding
      function PBT (O : Also_Crashes) return String is ("PBT");
   end Generic_Pkg;
end Test_Defs;
