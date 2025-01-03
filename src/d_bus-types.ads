pragma Ada_2022;

with Ada.Containers.Indefinite_Hashed_Maps;
with Ada.Containers.Indefinite_Vectors;
with D_Bus.Type_Internals;
with Interfaces;

with Ada.Finalization;
with Ada.Containers.Indefinite_Doubly_Linked_Lists;
with Ada.Iterator_Interfaces;

private with Ada.Streams;
private with Ada.Containers.Indefinite_Holders;
private with Ada.Containers.Indefinite_Doubly_Linked_Lists;

package D_Bus.Types is
   pragma Assertion_Policy (Dynamic_Predicate => Check);

   ---------------------
   -- Fast Signatures --
   ---------------------
   subtype Single_Signature is D_Bus.Type_Internals.Single_Signature;
   --  Signature of a single, complete element
   subtype Contents_Signature is D_Bus.Type_Internals.Contents_Signature;
   --  Signature of the contents of a container or list

   ------------------
   -- Type Classes --
   ------------------
   subtype Root_Type is D_Bus.Type_Internals.Root_Type;
   subtype Basic_Type is D_Bus.Type_Internals.Basic_Type;
   subtype String_Type is D_Bus.Type_Internals.String_Type;
   type Container_Type is interface and Root_Type;

   pragma Warnings (Off, "not referenced");
   function Never_Equal (L, R : Root_Type'Class) return Boolean is (False);
   pragma Warnings (On);

   --------------------
   -- Argument Lists --
   --------------------
   package Argument_Lists is new Ada.Containers.Indefinite_Doubly_Linked_Lists
     (Root_Type'Class, Never_Equal);
   subtype Argument_List is Argument_Lists.List;
   function Signature (X : Argument_List) return Contents_Signature;

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
   --  TODO Needs special help

   ------------------
   -- String Types --
   ------------------
   package Strings is new String_Wrappers ("s", Interfaces.Unsigned_32);
   subtype D_String is Strings.Outer;

   package Object_Paths is new String_Wrappers ("o", Interfaces.Unsigned_32);
   function Valid_Path (X : Object_Paths.Outer) return Boolean;
   subtype Object_Path is Object_Paths.Outer
   with Dynamic_Predicate => Valid_Path (Object_Path);

   package Signatures is new String_Wrappers ("g", Interfaces.Unsigned_8);
   function Valid_Signature (X : Signatures.Outer) return Boolean;
   subtype D_Signature is Signatures.Outer
   with Dynamic_Predicate => Valid_Signature (D_Signature);
   --  This signature type is SLOW, but validates all data passed to it.
   --  Use it when reading and writing to the D-Bus

   ----------------
   -- Containers --
   ----------------
--   type Dict is new Iterable_Container_Type with private;
--   function Element (X : Dict; K : Basic_Type'Class) return Root_Type'Class;
--   procedure Append (K : Basic_Type'Class; V : Root_Type'Class);

   
   ------------------------------------
   -- Containers for Root_Type'Class --
   ------------------------------------
   package Root_Type_Vectors is new Ada.Containers.Indefinite_Vectors
     (Natural, Root_Type'Class, Never_Equal);
   package Root_Type_Holders
      is new Ada.Containers.Indefinite_Holders (Root_Type'Class, Never_Equal);

   -------------
   -- Structs --
   -------------
   type Fixed_Index_Container_Type is interface and Container_Type;
   function Get
     (Container : Fixed_Index_Container_Type;
      Index : Natural) return Root_Type'Class is abstract;

   procedure Set
     (Container : Fixed_Index_Container_Type;
      Index : Natural;
      Value : Root_Type'Class) is abstract;

   generic
      Inner_Signature : Contents_Signature;
   package Structs is
      type Struct is new Fixed_Index_Container_Type with private;

      overriding
      function Signature return Single_Signature is
         ("(" & Inner_Signature & ")");

      overriding
      function Get
        (Container : Struct;
         Index : Natural) return Root_Type'Class;

      overriding
      procedure Set
        (Container : Struct;
         Index : Natural;
         Value : Root_Type'Class);
   private
      type Inner_Types is array (Natural range <>)
         of Unbounded_Single_Signature;
      type Inner_Array is array (Natural range <>) of Root_Type_Holders.Holder;

      Types : constant Inner_Types := Split_Signature (Inner_Signature);

      type Struct is new Fixed_Index_Container_Type with record
         Contents : Inner_Array (1 .. Types'Length);
      end record;
   end Structs;

   ------------
   -- Arrays --
   ------------
   type Container_Cursor is private;

   function Has_Element (C : Container_Cursor) return Boolean;

   package Container_Iterator is new Ada.Iterator_Interfaces (Container_Cursor, Has_Element);
   type Iterable_Container_Type
      is interface
         and Container_Iterator.Reversible_Iterator and Container_Type
   with
      Constant_Indexing => Constant_Reference,
      Variable_Indexing => Reference;

   type Constant_Reference_Type
     (X : not null access constant Root_Type'Class) is abstract tagged null record
     with
      Implicit_Dereference => X;
   type Reference_Type
      (X : not null access Root_Type'Class) is abstract tagged null record
      with Implicit_Dereference => X;

   function Constant_Reference
     (Container : Iterable_Container_Type'Class; Index : Natural)
     return Constant_Reference_Type is abstract;

   function Reference
     (Container : Iterable_Container_Type'Class; Index : Natural)
     return Reference_Type is abstract;

   generic
      Inner_Signature : Single_Signature;
      --  TODO ensure is only one complete type
      --  Inner signature - the signature of each element.
   package D_Arrays is
      type D_Array is new Iterable_Container_Type with private;
      overriding function Signature (X : D_Array) return String
      is (Inner_Signature);

   overriding
   function First
     (Object : D_Array) return Container_Cursor;

   overriding
   function Next
     (Object : D_Array;
      Position : Container_Cursor) return Container_Cursor;

   overriding
   function Last
     (Object : D_Array) return Container_Cursor;

   overriding
   function Previous
     (Object : D_Array;
      Position : Container_Cursor) return Container_Cursor;

   private
      type D_Array is new Root_Type_Vectors.Vector and Iterable_Container_Type
         with null record;
   end D_Arrays;

   -----------
   -- Dicts --
   -----------
   type Keyed_Iterable_Container_Type is interface and Iterable_Container_Type;
   --  TODO complete
   generic
      type Key_Type is new Basic_Type with private;
      Value_Signature : Single_Signature;
   package Dicts is
      type Dict is new Container_Type with private;

      overriding
      function Signature (X : Dict) return Single_Signature;
   private
      package Hash_Maps is new Ada.Containers.Indefinite_Hashed_Maps
        (Key_Type, Root_Type'Class);
      type Dict is new Hash_Maps.Map and Keyed_Iterable_Container_Type
         with null record;
   end Dicts;

   --------------
   -- Variants --
   --------------
   type Variant is new Container_Type with private;
   --  Note: Raises `Initialisation_Required` if left
   --  uninitialised!

   function "+" (X : Root_Type'Class) return Variant;
   function Get (X : Variant) return Root_Type'Class;
   function "+" (X : Variant) return Root_Type'Class renames Get;
private
   type Container_Cursor is record
      Inner : Root_Type_Vectors.Cursor;
   end record;

   ----------------
   -- Containers --
   ----------------
   type Variant is new Container_Type with record
      I : Root_Type_Holders.Holder := raise Initialisation_Required;
   end record;

   overriding function Signature (X : Variant) return String
   is ("v" & X.I.Element.Signature);
   procedure Read_Variant
     (Stream : not null access Ada.Streams.Root_Stream_Type'Class;
      Item : out Variant);
   procedure Write_Variant
     (Stream : not null access Ada.Streams.Root_Stream_Type'Class;
      Item : Variant);
   for Variant'Read use Read_Variant;
   for Variant'Write use Write_Variant;
end D_Bus.Types;
