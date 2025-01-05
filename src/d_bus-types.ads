pragma Ada_2022;

with Ada.Containers.Indefinite_Hashed_Maps;
with Ada.Containers.Indefinite_Vectors;

with Ada.Finalization;
with Ada.Containers.Indefinite_Holders;
with Ada.Containers.Indefinite_Doubly_Linked_Lists;
with Ada.Iterator_Interfaces;

with Interfaces;
with D_Bus.Type_Internals;
with D_Bus.Type_Internals.Containers;

private with Ada.Streams;
private with GNATCOLL.Strings;

package D_Bus.Types is
   pragma Assertion_Policy (Dynamic_Predicate => Check);

   -------------------------
   -- Ada Signature Types --
   -------------------------
   subtype Single_Signature is D_Bus.Type_Internals.Single_Signature;
   --  Signature of a single, complete element

   subtype Contents_Signature is D_Bus.Type_Internals.Contents_Signature;
   --  Signature of the contents of a container or list
   --  A `Single_Signature` may be converted to a `Contents_Signature`,
   --  but the reverse is not guaranteed to succeed.

   ------------------
   -- Type Classes --
   ------------------
   subtype Root_Type is D_Bus.Type_Internals.Root_Type;
   --  Any D_Bus type
   subtype Basic_Type is D_Bus.Type_Internals.Basic_Type;
   --  Any D_Bus type with statically-known contents
   --  (string types are included)
   subtype Container_Type is D_Bus.Type_Internals.Container_Type;
   --  Any D_Bus type with dynamic contents

   --------------------
   -- Argument Lists --
   --------------------
   pragma Warnings (Off, "not referenced");
   function Never_Equal (L, R : Root_Type'Class) return Boolean is (False);
   pragma Warnings (On);

   package Argument_Lists is new Ada.Containers.Indefinite_Doubly_Linked_Lists
     (Root_Type'Class, Never_Equal);
   subtype Argument_List is Argument_Lists.List;
   function Signature (X : Argument_List) return Contents_Signature;

   -----------------
   -- Fixed Types --
   -----------------
   generic package Fixed_Wrappers renames D_Bus.Type_Internals.Fixed_Wrappers;
   --  Use this to produce new types with a size known in advance

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
      renames D_Bus.Type_Internals.String_Wrappers;
   --  Use this to produce new types based on a String

   package Strings is new String_Wrappers
     (Type_Code => "s",
      Data_Length_Type => Interfaces.Unsigned_32,
      Inner_Type => String);
   subtype D_String is Strings.Outer;

   subtype Object_Path is D_Bus.Type_Internals.U_Object_Path;

   package Object_Paths is new String_Wrappers
     (Type_Code => "o",
      Data_Length_Type => Interfaces.Unsigned_32,
      Inner_Type => Object_Path);
   subtype D_Object_Path is Object_Paths.Outer;
   --  For a lightweight Ada type, use `Object_Path`

   package Signatures is new String_Wrappers
     (Type_Code => "g",
      Data_Length_Type => Interfaces.Unsigned_8,
      Inner_Type => Contents_Signature);
   subtype D_Signature is Signatures.Outer;
   --  For a lightweight Ada type, use `Single_Signature`
   --  or `Contents_Signature`

   ---------------------
   -- Container Types --
   ---------------------
   subtype Fixed_Container_Type is D_Bus.Type_Internals.Containers.Fixed_Container_Type; 
   generic package Structs renames D_Bus.Type_Internals.Containers.Structs;
   --  All D-Bus structs are instances of this type
   --  At runtime the actual type is unlikely to be known,
   --  use `Fixed_Container_Type'Class`

   subtype Numeric_Iterable_Container_Type
      is D_Bus.Type_Internals.Containers.Numeric_Iterable_Container_Type;
   generic package Arrays renames D_Bus.Type_Internals.Containers.Arrays;
   --  All D-Bus arrays are instances of this type.
   --  At runtime the actual type is unlikely to be known,
   --  use `Numeric_Iterable_Container_Type'Class`

   subtype Keyed_Iterable_Container_Type
      is D_Bus.Type_Internals.Containers.Keyed_Iterable_Container_Type;
   generic package Dicts renames D_Bus.Type_Internals.Containers.Dicts;
   --  All D-Bus dicts are generic instances of this type.
   --  At runtime the actual type is unlikely to be known,
   --  use `Keyed_Iterable_Container_Type'Class`

   subtype Variant is D_Bus.Type_Internals.Containers.Variant;
end D_Bus.Types;
