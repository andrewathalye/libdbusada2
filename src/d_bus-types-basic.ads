pragma Ada_2012;

with D_Bus.Streams;
with Interfaces;
with Ada.Strings.UTF_Encoding;
with GNAT.OS_Lib;

with D_Bus.Types.Basic_Generic; use D_Bus.Types.Basic_Generic;

package D_Bus.Types.Basic is
   -----------------
   -- Fixed Types --
   -----------------
   package Bytes is new Discrete_Wrappers (Byte_CC, Interfaces.Unsigned_8);
   type Byte is new Bytes.Outer with null record;
   for Byte'External_Tag use "D_Bus_Type_" & Byte_CC;

   type Boolean_32 is new Boolean with Size => 32;

   package Booleans is new Discrete_Wrappers (Boolean_CC, Boolean_32);
   type D_Boolean is new Booleans.Outer with null record;
   for D_Boolean'External_Tag use "D_Bus_Type_" & Boolean_CC;

   D_True  : constant D_Boolean := "True";
   D_False : constant D_Boolean := "False";

   package Int16s is new Discrete_Wrappers (Int16_CC, Interfaces.Integer_16);
   type Int16 is new Int16s.Outer with null record;
   for Int16'External_Tag use "D_Bus_Type_" & Int16_CC;

   package Uint16s is new
     Discrete_Wrappers (Uint16_CC, Interfaces.Unsigned_16);
   type Uint16 is new Uint16s.Outer with null record;
   for Uint16'External_Tag use "D_Bus_Type_" & Uint16_CC;

   package Int32s is new Discrete_Wrappers (Int32_CC, Interfaces.Integer_32);
   type Int32 is new Int32s.Outer with null record;
   for Int32'External_Tag use "D_Bus_Type_" & Int32_CC;

   package Uint32s is new
     Discrete_Wrappers (Uint32_CC, Interfaces.Unsigned_32);
   type Uint32 is new Uint32s.Outer with null record;
   for Uint32'External_Tag use "D_Bus_Type_" & Uint32_CC;

   function "+" (X : Interfaces.Unsigned_32) return Uint32
   is (Uint32s."+" (X) with null record);

   package Int64s is new Discrete_Wrappers (Int64_CC, Interfaces.Integer_64);
   type Int64 is new Int64s.Outer with null record;
   for Int64'External_Tag use "D_Bus_Type_" & Int64_CC;

   package Uint64s is new
     Discrete_Wrappers (Uint64_CC, Interfaces.Unsigned_64);
   type Uint64 is new Uint64s.Outer with null record;
   for Uint64'External_Tag use "D_Bus_Type_" & Uint64_CC;

   package Doubles is new Real_Wrappers (Double_CC, Interfaces.IEEE_Float_64);
   type Double is new Doubles.Outer with null record;
   for Double'External_Tag use "D_Bus_Type_" & Double_CC;

   ----------------------
   -- File Descriptors --
   ----------------------
   package File_Descriptors is new
     Discrete_Wrappers (File_Descriptor_CC, GNAT.OS_Lib.File_Descriptor);
   type File_Descriptor is new File_Descriptors.Outer with null record;
   for File_Descriptor'External_Tag use "D_Bus_Type_" & File_Descriptor_CC;
   --  Note: This only reads the _Index_ of a file descriptor from the D-Bus
   --  stream.
   --
   --  To obtain the actual file descriptor, call Store or Redeem
   function "+" (X : GNAT.OS_Lib.File_Descriptor) return File_Descriptor
   is (File_Descriptors."+" (X) with null record);

   procedure Redeem
     (X : in out File_Descriptor;
      S : not null access D_Bus.Streams.Possible_Alignable_Stream);
   --  Fetch a real file descriptor from the Stream by its Index

   procedure Store
     (X : in out File_Descriptor;
      S : not null access D_Bus.Streams.Possible_Alignable_Stream);
   --  Store a file descriptor in a Stream and store its index in the object

   ------------------
   -- String Types --
   ------------------
   package Strings is new
     String_Wrappers
       (Type_Code        => String_CC,
        Data_Length_Type => Interfaces.Unsigned_32,
        External_Type    => Ada.Strings.UTF_Encoding.UTF_8_String);
   type D_String is new Strings.Outer with null record;
   for D_String'External_Tag use "D_Bus_Type_" & String_CC;
   --  A string must NOT contain NUL and must also be a valid UTF 8 string
   --  The exception Invalid_D_Bus_String will be raised if either check fails.

   Invalid_D_Bus_String : exception renames Basic_Generic.Invalid_D_Bus_String;

   function "+" (X : String) return D_String
   is (Strings."+" (X) with null record);

   type U_Object_Path is new String;
   function Validate_Object_Path (X : U_Object_Path) return Boolean;
   subtype Object_Path is U_Object_Path
   with
     Dynamic_Predicate => Validate_Object_Path (Object_Path),
     Predicate_Failure => "Invalid object path " & String (Object_Path);

   package Object_Paths is new
     String_Wrappers
       (Type_Code        => Object_Path_CC,
        Data_Length_Type => Interfaces.Unsigned_32,
        External_Type    => Object_Path);
   type D_Object_Path is new Object_Paths.Outer with null record;
   for D_Object_Path'External_Tag use "D_Bus_Type_" & Object_Path_CC;
   --  For a lightweight Ada type, use `Object_Path`

   function "+" (X : Object_Path) return D_Object_Path
   is (Object_Paths."+" (X) with null record);

   package Signatures is new
     String_Wrappers
       (Type_Code        => Signature_CC,
        Data_Length_Type => Interfaces.Unsigned_8,
        External_Type    => Contents_Signature);
   type D_Signature is new Signatures.Outer with null record;
   for D_Signature'External_Tag use "D_Bus_Type_" & Signature_CC;
   --  For a lightweight Ada type, use `Single_Signature`
   --  or `Contents_Signature`

   function "+" (X : Contents_Signature) return D_Signature
   is (Signatures."+" (X) with null record);
end D_Bus.Types.Basic;
