pragma Ada_2012;

with Interfaces;

with D_Bus.Types.Basic; use D_Bus.Types.Basic;

package D_Bus.Types.Instances is
   -----------------
   -- Fixed Types --
   -----------------
   package Bytes is new Fixed_Wrappers (Byte_CC, Interfaces.Unsigned_8);
   subtype Byte is Bytes.Outer;

   type Boolean_32 is new Boolean
      with Size => 32;
   package Booleans is new Fixed_Wrappers (Boolean_CC, Boolean_32);
   subtype D_Boolean is Booleans.Outer;
   function "+" (X : D_Boolean) return Boolean is (Boolean (Booleans."+" (X)));
   function "+" (X : Boolean) return D_Boolean
      is (Booleans."+" (Boolean_32 (X)));

   package Int16s is new Fixed_Wrappers (Int16_CC, Interfaces.Integer_16);
   subtype Int16 is Int16s.Outer;
   package Uint16s is new Fixed_Wrappers (Uint16_CC, Interfaces.Unsigned_16);
   subtype Uint16 is Uint16s.Outer;

   package Int32s is new Fixed_Wrappers (Int32_CC, Interfaces.Integer_32);
   subtype Int32 is Int32s.Outer;
   package Uint32s is new Fixed_Wrappers (Uint32_CC, Interfaces.Unsigned_32);
   subtype Uint32 is Uint32s.Outer;

   package Int64s is new Fixed_Wrappers (Int64_CC, Interfaces.Integer_64);
   subtype Int64 is Int64s.Outer;
   package Uint64s is new Fixed_Wrappers (Uint64_CC, Interfaces.Unsigned_64);
   subtype Uint64 is Uint64s.Outer;

   package Doubles is new Fixed_Wrappers (Double_CC, Interfaces.IEEE_Float_64);
   subtype Double is Doubles.Outer;

   package File_Descriptors is new Fixed_Wrappers
     (File_Descriptor_CC, Integer);
   subtype File_Descriptor is File_Descriptors.Outer;
   --  TODO Needs special help. this is wrong

   ------------------
   -- String Types --
   ------------------
   package Strings is new String_Wrappers
     (Type_Code => String_CC,
      Data_Length_Type => Interfaces.Unsigned_32,
      External_Type => String);
   subtype D_String is Strings.Outer;

   subtype Object_Path is D_Bus.Types.U_Object_Path;

   package Object_Paths is new String_Wrappers
     (Type_Code => Object_Path_CC,
      Data_Length_Type => Interfaces.Unsigned_32,
      External_Type => Object_Path);
   subtype D_Object_Path is Object_Paths.Outer;
   --  For a lightweight Ada type, use `Object_Path`

   package Signatures is new String_Wrappers
     (Type_Code => Signature_CC,
      Data_Length_Type => Interfaces.Unsigned_8,
      External_Type => Contents_Signature);
   subtype D_Signature is Signatures.Outer;
   --  For a lightweight Ada type, use `Single_Signature`
   --  or `Contents_Signature`
end D_Bus.Types.Instances;
