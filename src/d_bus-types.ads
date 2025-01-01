pragma Ada_2022;

with D_Bus.Type_Internals;
with Interfaces;

with Ada.Finalization;
private with Ada.Streams;

package D_Bus.Types is
   pragma Assertion_Policy (Dynamic_Predicate => Check);

   ------------------
   -- Type Classes --
   ------------------
   subtype Root_Type is D_Bus.Type_Internals.Root_Type;
   subtype Basic_Type is D_Bus.Type_Internals.Basic_Type;
   subtype String_Type is D_Bus.Type_Internals.String_Type;
   type Container_Type is abstract new Ada.Finalization.Controlled
      and Root_Type with private;

   --------------
   -- Wrappers --
   --------------
   generic package Basic_Wrappers renames D_Bus.Type_Internals.Basic_Wrappers;
   generic package String_Wrappers
      renames D_Bus.Type_Internals.String_Wrappers;

   -----------------
   -- Basic Types --
   -----------------
   package Bytes is new Basic_Wrappers
     (Tag   => "y",
      Inner => Interfaces.Unsigned_8);
   subtype Byte is Bytes.Outer;

   type Boolean_32 is new Boolean
      with Size => 32;
   package Booleans is new Basic_Wrappers ("b", Boolean_32);
   subtype D_Boolean is Booleans.Outer;
   function "+" (X : D_Boolean) return Boolean is (Boolean (Booleans."+" (X)));
   function "+" (X : Boolean) return D_Boolean
      is (Booleans."+" (Boolean_32 (X)));

   package Int16s is new Basic_Wrappers ("q", Interfaces.Integer_16);
   subtype Int16 is Int16s.Outer;
   package Uint16s is new Basic_Wrappers ("n", Interfaces.Unsigned_16);
   subtype Uint16 is Uint16s.Outer;

   package Int32s is new Basic_Wrappers ("i", Interfaces.Integer_32);
   subtype Int32 is Int32s.Outer;
   package Uint32s is new Basic_Wrappers ("u", Interfaces.Unsigned_32);
   subtype Uint32 is Uint32s.Outer;

   package Int64s is new Basic_Wrappers ("x", Interfaces.Integer_64);
   subtype Int64 is Int64s.Outer;
   package Uint64s is new Basic_Wrappers ("t", Interfaces.Unsigned_64);
   subtype Uint64 is Uint64s.Outer;

   package Doubles is new Basic_Wrappers ("d", Interfaces.IEEE_Float_64);
   subtype Double is Doubles.Outer;
--   type File_Descriptor is new Basic_Type with private;
   --  Needs special help

   ------------------
   -- String Types --
   ------------------
   package Strings is new String_Wrappers ("s", Interfaces.Unsigned_32);
   subtype D_String is Strings.Outer;

   package Object_Paths is new String_Wrappers ("o", Interfaces.Unsigned_32);
   function Valid (X : Object_Paths.Outer) return Boolean;
   subtype Object_Path is Object_Paths.Outer
   with Dynamic_Predicate => Valid (Object_Path);

   package Signatures is new String_Wrappers ("g", Interfaces.Unsigned_8);
   function Valid (X : Signatures.Outer) return Boolean;
   subtype D_Signature is Signatures.Outer
   with Dynamic_Predicate => Valid (D_Signature);

   ----------------
   -- Containers --
   ----------------
--   type Struct is new Iterable_Container_Type with private;
   --  Iterable? Signature?

--   type Dict is new Iterable_Container_Type with private;
--   function Element (X : Dict; K : Basic_Type'Class) return Root_Type'Class;
--   procedure Append (K : Basic_Type'Class; V : Root_Type'Class);

--   type D_Array is new Iterable_Container_Type with private;

   type U_Variant is new Container_Type with private;
   function Valid (X : U_Variant) return Boolean;
   subtype Variant is U_Variant
   with Dynamic_Predicate => Valid (Variant);
   function "+" (X : Root_Type'Class) return U_Variant;
   function "+" (X : U_Variant) return Root_Type'Class;
   --  TODO we need a way to deallocate this
private

   ----------------
   -- Containers --
   ----------------
   type Container_Type is abstract new Ada.Finalization.Controlled
      and Root_Type
   with null record;

   type Root_Type_Access is access Root_Type'Class;

   type U_Variant is new Container_Type with record
      I : Root_Type_Access;
   end record;
   overriding function Signature (X : U_Variant) return String
   is ("v" & X.I.Signature);
   procedure Read_Variant
     (Stream : not null access Ada.Streams.Root_Stream_Type'Class;
      Item : out U_Variant);
   procedure Write_Variant
     (Stream : not null access Ada.Streams.Root_Stream_Type'Class;
      Item : U_Variant);
   for U_Variant'External_Tag use "v";
   for U_Variant'Read use Read_Variant;
   for U_Variant'Write use Write_Variant;
end D_Bus.Types;
