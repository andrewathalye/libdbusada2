pragma Ada_2012;

with Ada.Containers.Indefinite_Holders;
with Ada.Streams;
with Ada.Iterator_Interfaces;

private with Ada.Containers.Indefinite_Vectors;
private with Ada.Containers.Indefinite_Hashed_Maps;

package D_Bus.Types.Containers is
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
     (Container : out Fixed_Container_Type;
      Index : Natural;
      Value : Root_Type'Class) is abstract;

   function Count
     (Container : Fixed_Container_Type) return Natural is abstract;

   generic
      Inner_Signature : Contents_Signature;
   package Structs is
      type Struct is new Fixed_Container_Type with private;
      --  Note: All elements of `Struct` must be assigned
      --  values before `Struct` can be written or its values
      --  used. This can be achieved by reading it from a Stream
      --  or calling `Set` manually.
      --  TODO enforce this or we might end up with problems
   private
      type Inner_Array is array (Natural range <>) of Root_Type_Holders.Holder;

      Types : constant D_Bus.Types.Single_Signature_Array :=
        D_Bus.Types.Split_Signature (Inner_Signature);
      --  Shared for all objects of this type

      type Struct is new Fixed_Container_Type with record
         Inner : Inner_Array (1 .. Types'Length);
      end record;

      procedure Read
        (Stream : not null access Ada.Streams.Root_Stream_Type'Class;
         Item : out Struct);

      procedure Write
        (Stream : not null access Ada.Streams.Root_Stream_Type'Class;
         Item : Struct);

      for Struct'Read use Read;
      for Struct'Write use Write;

      overriding
      function Contents (X : Struct) return Contents_Signature
      is (Inner_Signature);

      overriding
      function Signature (X : Struct) return Single_Signature
      is ("(" & Single_Signature (X.Contents) & ")");

      overriding
      function Size (X : Struct) return Ada.Streams.Stream_Element_Count;

      overriding
      function Image (X : Struct) return String;

      overriding
      function Get
        (Container : Struct;
         Index : Natural) return Root_Type'Class
      is (Container.Inner (Index).Element);

      overriding
      procedure Set
        (Container : out Struct;
         Index : Natural;
         Value : Root_Type'Class);

      overriding
      function Count (Container : Struct) return Natural
      is (Container.Inner'Length);
   end Structs;

   --------------------------------
   -- Iterable Containers Shared --
   --------------------------------
   type Constant_Reference_Type (X : not null access constant Root_Type'Class)
   is limited private
   with
      Implicit_Dereference => X;

   type Reference_Type (X : not null access Root_Type'Class)
   is limited private
   with Implicit_Dereference => X;

   --------------------------
   --       Arrays         --
   -- (Numeric Containers) --
   --------------------------
   type Numeric_Container_Cursor (<>) is private;
   No_Index : constant Numeric_Container_Cursor;

   function Has_Element (C : Numeric_Container_Cursor) return Boolean;

   package Numeric_Container_Iterators is new Ada.Iterator_Interfaces
     (Numeric_Container_Cursor, Has_Element);
   subtype Numeric_Container_Iterator
   is Numeric_Container_Iterators.Reversible_Iterator;

   type Numeric_Container_Type is interface
      and Numeric_Container_Iterator
      and Container_Type
   with
      Constant_Indexing => Constant_Reference_NCT,
      Variable_Indexing => Reference_NCT,
      Default_Iterator => Iterate_NCT,
      Iterator_Element => Root_Type'Class;

   function Constant_Reference_NCT
     (Container : aliased Numeric_Container_Type;
      Index : Natural) return Constant_Reference_Type is abstract;

   function Constant_Reference_NCT
     (Container : aliased Numeric_Container_Type;
     Cursor : Numeric_Container_Cursor) return Constant_Reference_Type
   is abstract;

   function Reference_NCT
     (Container : aliased in out Numeric_Container_Type;
      Index : Natural) return Reference_Type is abstract;

   function Reference_NCT
     (Container : aliased in out Numeric_Container_Type;
      Cursor : Numeric_Container_Cursor) return Reference_Type is abstract;

   function Has_Element
     (Container : Numeric_Container_Type;
      Index : Natural) return Boolean is abstract;

   function Index (C : Numeric_Container_Cursor) return Natural;
   function Element (C : Numeric_Container_Cursor) return Root_Type'Class;
   function Iterate_NCT (Container : Numeric_Container_Type'Class)
      return Numeric_Container_Iterator'Class is (Container);

   generic
      Inner_Signature : Single_Signature;
   package Arrays is
      type D_Array is new Numeric_Container_Type with private;
   private
      package Vectors is new Ada.Containers.Indefinite_Vectors
        (Natural, Root_Type'Class);

      type D_Array is new Numeric_Container_Type with record
         Inner : Vectors.Vector;
      end record;

      procedure Read
        (Stream : not null access Ada.Streams.Root_Stream_Type'Class;
         Item : out D_Array);

      procedure Write
        (Stream : not null access Ada.Streams.Root_Stream_Type'Class;
         Item : D_Array);

      for D_Array'Read use Read;
      for D_Array'Write use Write;

      overriding
      function Contents (X : D_Array) return Contents_Signature
      is (Contents_Signature (Inner_Signature));

      overriding
      function Signature (X : D_Array) return Single_Signature
      is ("a" & Single_Signature (X.Contents));

      overriding
      function Size (X : D_Array) return Ada.Streams.Stream_Element_Count;

      overriding
      function Image (X : D_Array) return String;

      overriding
      function First
        (Object : D_Array) return Numeric_Container_Cursor;

      overriding
      function Next
        (Object : D_Array;
         Position : Numeric_Container_Cursor) return Numeric_Container_Cursor;

      overriding
      function Last
        (Object : D_Array) return Numeric_Container_Cursor;

      overriding
      function Previous
        (Object : D_Array;
         Position : Numeric_Container_Cursor) return Numeric_Container_Cursor;

      overriding
      function Constant_Reference_NCT
        (Container : aliased D_Array;
         Index : Natural) return Constant_Reference_Type;

      overriding
      function Constant_Reference_NCT
        (Container : aliased D_Array;
         C : Numeric_Container_Cursor) return Constant_Reference_Type;

      overriding
      function Reference_NCT
        (Container : aliased in out D_Array;
         Index : Natural) return Reference_Type;

      overriding
      function Reference_NCT
        (Container : aliased in out D_Array;
        C : Numeric_Container_Cursor) return Reference_Type;

      overriding
      function Has_Element
        (Container : D_Array; Index : Natural) return Boolean;
   end Arrays;

   ------------------------
   --       Dicts        --
   -- (Keyed Containers) --
   ------------------------
   --  TODO reconsider internal representation. might be faster to use a{KV}
   type Keyed_Container_Cursor (<>) is private;
   No_Key : constant Keyed_Container_Cursor;

   function Has_Element (C : Keyed_Container_Cursor) return Boolean;

   package Keyed_Container_Iterators is new Ada.Iterator_Interfaces
     (Keyed_Container_Cursor, Has_Element);
   subtype Keyed_Container_Iterator
   is Keyed_Container_Iterators.Forward_Iterator;

   type Keyed_Container_Type is interface
      and Keyed_Container_Iterator
      and Container_Type
   with
      Constant_Indexing => Constant_Reference_KCT,
      Variable_Indexing => Reference_KCT,
      Default_Iterator => Iterate_KCT,
      Iterator_Element => Root_Type'Class;

   function Constant_Reference_KCT
     (Container : aliased Keyed_Container_Type;
      Key : Basic_Type'Class) return Constant_Reference_Type is abstract;

   function Constant_Reference_KCT
     (Container : aliased Keyed_Container_Type;
      Cursor : Keyed_Container_Cursor) return Constant_Reference_Type
    is abstract;

   function Reference_KCT
     (Container : aliased in out Keyed_Container_Type;
      Key : Basic_Type'Class) return Reference_Type is abstract;

   function Reference_KCT
     (Container : aliased in out Keyed_Container_Type;
      Cursor : Keyed_Container_Cursor) return Reference_Type is abstract;

   procedure Insert
     (Container : in out Keyed_Container_Type;
      Key : Basic_Type'Class;
      Value : Root_Type'Class) is abstract;

   function Has_Element
     (Container : Keyed_Container_Type;
      Key : Basic_Type'Class) return Boolean is abstract;

   function Key (C : Keyed_Container_Cursor) return Basic_Type'Class;
   function Element (C : Keyed_Container_Cursor) return Root_Type'Class;
   function Iterate_KCT (Container : Keyed_Container_Type'Class)
      return Keyed_Container_Iterator'Class is (Container);

   generic
      Key_Type_Code : Signature_Element;
      Value_Signature : Single_Signature;
   package Dicts is
      type Dict is new Keyed_Container_Type with private;
   private
      function Hash (Key : Basic_Type'Class) return Ada.Containers.Hash_Type;

      package Hash_Maps is new Ada.Containers.Indefinite_Hashed_Maps
        (Key_Type        => Basic_Type'Class,
         Element_Type    => Root_Type'Class,
         Hash            => Hash,
         Equivalent_Keys => "=");

      type Dict is new Keyed_Container_Type with record
         Inner : Hash_Maps.Map;
      end record;

      procedure Read
        (Stream : not null access Ada.Streams.Root_Stream_Type'Class;
         Item : out Dict);

      procedure Write
        (Stream : not null access Ada.Streams.Root_Stream_Type'Class;
         Item : Dict);

      for Dict'Read use Read;
      for Dict'Write use Write;

      overriding
      procedure Insert
        (Container : in out Dict;
         Key : Basic_Type'Class;
         Value : Root_Type'Class);

      overriding
      function Contents (X : Dict) return Contents_Signature
      is (Contents_Signature (Key_Type_Code & Value_Signature));

      overriding
      function Signature (X : Dict) return Single_Signature
      is ("a{" & Key_Type_Code & Single_Signature (X.Contents) & "}");

      overriding
      function Size (X : Dict) return Ada.Streams.Stream_Element_Count;

      overriding
      function Image (X : Dict) return String;

      overriding
      function First
        (Object : Dict) return Keyed_Container_Cursor;

      overriding
      function Next
        (Object : Dict;
         Position : Keyed_Container_Cursor) return Keyed_Container_Cursor;

      overriding
      function Constant_Reference_KCT
        (Container : aliased Dict;
         Key : Basic_Type'Class) return Constant_Reference_Type;

      overriding
      function Constant_Reference_KCT
        (Container : aliased Dict;
         C : Keyed_Container_Cursor) return Constant_Reference_Type;

      overriding
      function Reference_KCT
        (Container : aliased in out Dict;
         Key : Basic_Type'Class) return Reference_Type;

      overriding
      function Reference_KCT
        (Container : aliased in out Dict;
         C : Keyed_Container_Cursor) return Reference_Type;

      overriding
      function Has_Element
        (Container : Dict;
         Key : Basic_Type'Class) return Boolean
      is (Container.Inner.Contains (Key));
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
   type Numeric_Container_Cursor is record
      Container : access constant Numeric_Container_Type'Class;
      Index : Integer;
   end record;

   type Keyed_Container_Cursor is record
      Container : access constant Keyed_Container_Type'Class;
      Key : access constant Basic_Type'Class;
   end record;

   No_Index : constant Numeric_Container_Cursor := (null, -1);
   No_Key : constant Keyed_Container_Cursor := (null, null);

   type Constant_Reference_Type
     (X : not null access constant Root_Type'Class) is limited null record;

   type Reference_Type
     (X : not null access Root_Type'Class) is limited null record;

   --------------
   -- Variants --
   --------------
   type Variant is new Container_Type with record
      I : Root_Type_Holders.Holder := raise Initialisation_Required;
   end record;

   overriding
   function Contents (X : Variant) return Contents_Signature
   is (Contents_Signature (X.I.Element.Signature));

   overriding
   function Signature (X : Variant) return Single_Signature
   is ("v" & Single_Signature (X.Contents));

   overriding
   function Size (X : Variant) return Ada.Streams.Stream_Element_Count;

   overriding
   function Image (X : Variant) return String;

   procedure Read_Variant
     (Stream : not null access Ada.Streams.Root_Stream_Type'Class;
      Item : out Variant);
   procedure Write_Variant
     (Stream : not null access Ada.Streams.Root_Stream_Type'Class;
      Item : Variant);
   for Variant'Read use Read_Variant;
   for Variant'Write use Write_Variant;
end D_Bus.Types.Containers;
