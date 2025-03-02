pragma Ada_2012;

with Interfaces;
with Ada.Strings.UTF_Encoding;
with GNAT.OS_Lib;

with D_Bus.Types.Basic_Generic; use D_Bus.Types.Basic_Generic;

package D_Bus.Types.Basic is
   -----------------
   -- Fixed Types --
   -----------------
   package Bytes is new Discrete_Wrappers (Byte_CC, Interfaces.Unsigned_8);
   subtype Byte is Bytes.Outer;

   type Boolean_32 is new Boolean with
     Size => 32;
   package Booleans is new Discrete_Wrappers (Boolean_CC, Boolean_32);
   subtype D_Boolean is Booleans.Outer;
   D_True  : constant D_Boolean := "True";
   D_False : constant D_Boolean := "False";

   function "+" (X : D_Boolean) return Boolean is (Boolean (Booleans."+" (X)));
   function "+" (X : Boolean) return D_Boolean is
     (Booleans."+" (Boolean_32 (X)));

   package Int16s is new Discrete_Wrappers (Int16_CC, Interfaces.Integer_16);
   subtype Int16 is Int16s.Outer;
   package Uint16s is new Discrete_Wrappers
     (Uint16_CC, Interfaces.Unsigned_16);
   subtype Uint16 is Uint16s.Outer;

   package Int32s is new Discrete_Wrappers (Int32_CC, Interfaces.Integer_32);
   subtype Int32 is Int32s.Outer;
   package Uint32s is new Discrete_Wrappers
     (Uint32_CC, Interfaces.Unsigned_32);
   subtype Uint32 is Uint32s.Outer;

   package Int64s is new Discrete_Wrappers (Int64_CC, Interfaces.Integer_64);
   subtype Int64 is Int64s.Outer;
   package Uint64s is new Discrete_Wrappers
     (Uint64_CC, Interfaces.Unsigned_64);
   subtype Uint64 is Uint64s.Outer;

   package Doubles is new Real_Wrappers (Double_CC, Interfaces.IEEE_Float_64);
   subtype Double is Doubles.Outer;

   package File_Descriptors is new Discrete_Wrappers
     (File_Descriptor_CC, GNAT.OS_Lib.File_Descriptor);
   subtype File_Descriptor is File_Descriptors.Outer;
   --  Note: This type is transparently remapped when reading Messages
   --  from a Connection. If you manually read one from a stream, the
   --  result will NOT correspond to a valid file descriptor.

   ------------------
   -- String Types --
   ------------------
   package Strings is new String_Wrappers
     (Type_Code     => String_CC, Data_Length_Type => Interfaces.Unsigned_32,
      External_Type => Ada.Strings.UTF_Encoding.UTF_8_String);
   subtype D_String is Strings.Outer;
   --  TODO a string must NOT contain NUL and must also be a valid UTF 8 string
   --  Is it worth it to enforce this? Probably

   type U_Object_Path is new String;
   function Validate_Object_Path (X : U_Object_Path) return Boolean;
   subtype Object_Path is U_Object_Path with
       Dynamic_Predicate => Validate_Object_Path (Object_Path),
       Predicate_Failure => "Invalid object path " & String (Object_Path);

   package Object_Paths is new String_Wrappers
     (Type_Code => Object_Path_CC, Data_Length_Type => Interfaces.Unsigned_32,
      External_Type => Object_Path);
   subtype D_Object_Path is Object_Paths.Outer;
   --  For a lightweight Ada type, use `Object_Path`

   package Signatures is new String_Wrappers
     (Type_Code     => Signature_CC, Data_Length_Type => Interfaces.Unsigned_8,
      External_Type => Contents_Signature);
   subtype D_Signature is Signatures.Outer;
   --  For a lightweight Ada type, use `Single_Signature`
   --  or `Contents_Signature`
end D_Bus.Types.Basic;
