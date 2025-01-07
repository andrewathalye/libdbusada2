pragma Ada_2012;

with Interfaces;

package D_Bus.Types.Instances is
   -----------------
   -- Fixed Types --
   -----------------
   package Bytes is new Fixed_Wrappers ("y", Interfaces.Unsigned_8);
   subtype Byte is Bytes.Outer;

   type Boolean_32 is new Boolean
      with Size => 32;
   package Booleans is new Fixed_Wrappers ("b", Boolean_32);
   subtype D_Boolean is Booleans.Outer;
   function "+" (X : D_Boolean) return Boolean is (Boolean (Booleans."+" (X)));
   function "+" (X : Boolean) return D_Boolean
      is (Booleans."+" (Boolean_32 (X)));

   package Int16s is new Fixed_Wrappers ("q", Interfaces.Integer_16);
   subtype Int16 is Int16s.Outer;
   package Uint16s is new Fixed_Wrappers ("n", Interfaces.Unsigned_16);
   subtype Uint16 is Uint16s.Outer;

   package Int32s is new Fixed_Wrappers ("i", Interfaces.Integer_32);
   subtype Int32 is Int32s.Outer;
   package Uint32s is new Fixed_Wrappers ("u", Interfaces.Unsigned_32);
   subtype Uint32 is Uint32s.Outer;

   package Int64s is new Fixed_Wrappers ("x", Interfaces.Integer_64);
   subtype Int64 is Int64s.Outer;
   package Uint64s is new Fixed_Wrappers ("t", Interfaces.Unsigned_64);
   subtype Uint64 is Uint64s.Outer;

   package Doubles is new Fixed_Wrappers ("d", Interfaces.IEEE_Float_64);
   subtype Double is Doubles.Outer;

   --   type File_Descriptor is new Basic_Type with private;
   --  TODO Needs special help

   ------------------
   -- String Types --
   ------------------
   generic package String_Wrappers
      renames D_Bus.Types.String_Wrappers;
   --  Use this to produce new types based on a String

   package Strings is new String_Wrappers
     (Type_Code => "s",
      Data_Length_Type => Interfaces.Unsigned_32,
      External_Type => String);
   subtype D_String is Strings.Outer;

   subtype Object_Path is D_Bus.Types.U_Object_Path;

   package Object_Paths is new String_Wrappers
     (Type_Code => "o",
      Data_Length_Type => Interfaces.Unsigned_32,
      External_Type => Object_Path);
   subtype D_Object_Path is Object_Paths.Outer;
   --  For a lightweight Ada type, use `Object_Path`

   package Signatures is new String_Wrappers
     (Type_Code => "g",
      Data_Length_Type => Interfaces.Unsigned_8,
      External_Type => Contents_Signature);
   subtype D_Signature is Signatures.Outer;
   --  For a lightweight Ada type, use `Single_Signature`
   --  or `Contents_Signature`
end D_Bus.Types.Instances;
