pragma Ada_2012;

with Ada.Containers.Indefinite_Holders;
with Ada.Streams;
with Ada.Iterator_Interfaces;

private with Ada.Containers.Vectors;
private with Ada.Containers.Hashed_Maps;

package D_Bus.Type_Internals.Containers is
   ---------------------
   -- Shared Packages --
   ---------------------
   package Root_Type_Holders is new Ada.Containers.Indefinite_Holders
     (Root_Type'Class);

   -------------
   -- Structs --
   -------------
   type Fixed_Container_Type is interface and Container_Type;
   function Get
     (Container : Fixed_Container_Type;
      Index : Natural) return Root_Type'Class is abstract;

   procedure Set
     (Container : Fixed_Container_Type;
      Index : Natural;
      Value : Root_Type'Class) is abstract;

   function Count
     (Container : Fixed_Container_Type) return Natural is abstract;

   generic
      Inner_Signature : Contents_Signature;
   package Structs is
      use type Single_Signature;

      type Struct is new Fixed_Container_Type with private;
      --  Note: All elements of `Struct` must be assigned
      --  values before `Struct` can be written or its values
      --  used. This can be achieved by reading it from a Stream
      --  or calling `Set` manually.

   private
      type Inner_Array is array (Natural range <>) of Root_Type_Holders.Holder;

      Types : constant D_Bus.Type_Internals.Single_Signature_Array :=
        D_Bus.Type_Internals.Split_Signature (Inner_Signature);
      --  Shared for all objects of this type

      type Struct is new Fixed_Container_Type with record
         Inner : Inner_Array (1 .. Types'Length);
      end record;

      overriding
      function Signature (X : Struct) return Single_Signature;

      overriding
      function Contents (X : Struct) return Contents_Signature;

      overriding
      function Get
        (Container : Struct;
         Index : Natural) return Root_Type'Class;

      overriding
      procedure Set
        (Container : Struct;
         Index : Natural;
         Value : Root_Type'Class);

      overriding
      function Count (Container : Struct) return Natural
      is (Container.Inner'Length);
   end Structs;

   -------------------------
   -- Iterable Containers --
   -------------------------
   type Container_Cursor is private;

   function Has_Element (C : Container_Cursor) return Boolean;

   package Container_Iterator is new Ada.Iterator_Interfaces
     (Container_Cursor, Has_Element);
   type Iterable_Container_Type
      is interface
         and Container_Iterator.Reversible_Iterator and Container_Type;

   type Constant_Reference_Type (X : not null access constant Root_Type'Class)
   is private
   with
      Implicit_Dereference => X;

   type Reference_Type (X : not null access Root_Type'Class)
   is private
   with Implicit_Dereference => X;

   -----------------------------------
   --            Arrays             --
   -- (Numeric Iterable Containers) --
   -----------------------------------
   --  TODO have indexing directly return Element_Type?
   --  Note: Elements may have indeterminate signature
   type Numeric_Iterable_Container_Type
   is interface and Iterable_Container_Type
   with
      Constant_Indexing => Constant_Reference_NICT,
      Variable_Indexing => Reference_NICT;

   function Constant_Reference_NICT
     (Container : Numeric_Iterable_Container_Type;
      Index : Natural) return Constant_Reference_Type is abstract;

   function Reference_NICT
     (Container : Numeric_Iterable_Container_Type;
      Index : Natural) return Reference_Type is abstract;

   generic
      Inner_Signature : Single_Signature;
      type Element_Type is new Root_Type with private;
   package Arrays is
      type D_Array is new Numeric_Iterable_Container_Type with private;
   private
      use type Single_Signature;

      package Vectors is new Ada.Containers.Vectors (Natural, Element_Type);

      type D_Array is new Numeric_Iterable_Container_Type with record
         Inner : Vectors.Vector;
      end record;

      overriding
      function Signature (X : D_Array) return Single_Signature;

      overriding
      function Contents (X : D_Array) return Contents_Signature;

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

      overriding
      function Constant_Reference_NICT
        (Container : D_Array; Index : Natural) return Constant_Reference_Type;

      overriding
      function Reference_NICT
        (Container : D_Array; Index : Natural) return Reference_Type;
   end Arrays;

   ---------------------------------
   --            Dicts            --
   -- (Keyed Iterable Containers) --
   ---------------------------------
   --  Note Elements may have indeterminate signature
   type Keyed_Iterable_Container_Type is interface and Iterable_Container_Type
   with
      Constant_Indexing => Constant_Reference_KICT,
      Variable_Indexing => Reference_KICT;

   function Constant_Reference_KICT
     (Container : Keyed_Iterable_Container_Type;
      Key : Basic_Type'Class) return Constant_Reference_Type is abstract;

   function Reference_KICT
     (Container : Keyed_Iterable_Container_Type;
      Key : Basic_Type'Class) return Reference_Type is abstract;

   procedure Insert
     (Container : in out Keyed_Iterable_Container_Type;
      Key : Basic_Type'Class;
      New_Item : Root_Type'Class) is abstract;

   generic
      type Key_Type is new Basic_Type with private;

      Value_Signature : Single_Signature;
      type Value_Type is new Root_Type with private;
   package Dicts is
      type Dict is new Keyed_Iterable_Container_Type with private;
   private
      function Hash (Key : Key_Type) return Ada.Containers.Hash_Type;

      package Hash_Maps is new Ada.Containers.Hashed_Maps
        (Key_Type        => Key_Type,
         Element_Type    => Value_Type,
         Hash            => Hash,
         Equivalent_Keys => "=");

      type Dict is new Keyed_Iterable_Container_Type with record
         Inner : Hash_Maps.Map;
      end record;

      overriding
      procedure Insert
        (Container : in out Dict;
         Key : Basic_Type'Class;
         New_Item : Root_Type'Class);

      overriding
      function Signature (X : Dict) return Single_Signature;

      overriding
      function Contents (X : Dict) return Contents_Signature;

      overriding
      function First
        (Object : Dict) return Container_Cursor;

      overriding
      function Next
        (Object : Dict;
         Position : Container_Cursor) return Container_Cursor;

      overriding
      function Last
        (Object : Dict) return Container_Cursor;

      overriding
      function Previous
        (Object : Dict;
         Position : Container_Cursor) return Container_Cursor;

      overriding
      function Constant_Reference_KICT
        (Container : Dict;
         Key : Basic_Type'Class) return Constant_Reference_Type;

      overriding
      function Reference_KICT
        (Container : Dict;
         Index : Basic_Type'Class) return Reference_Type;

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
   ----------------
   -- Containers --
   ----------------
   type Index_Storage is mod 2 ** 32;
   for Index_Storage'Size use 32;

   type Container_Cursor is record
      Container : not null access Iterable_Container_Type'Class;
      Index : Index_Storage;
   end record;

   type Constant_Reference_Type
     (X : not null access constant Root_Type'Class) is null record;

   type Reference_Type
     (X : not null access Root_Type'Class) is null record;

   --------------
   -- Variants --
   --------------
   type Variant is new Container_Type with record
      I : Root_Type_Holders.Holder := raise Initialisation_Required;
   end record;

   overriding
   function Signature (X : Variant) return Single_Signature;

   overriding
   function Contents (X : Variant) return Contents_Signature;

   procedure Read_Variant
     (Stream : not null access Ada.Streams.Root_Stream_Type'Class;
      Item : out Variant);
   procedure Write_Variant
     (Stream : not null access Ada.Streams.Root_Stream_Type'Class;
      Item : Variant);
   for Variant'Read use Read_Variant;
   for Variant'Write use Write_Variant;
end D_Bus.Type_Internals.Containers;
