pragma Ada_2012;

with Ada.Iterator_Interfaces;

with Ada.Streams;

private with Ada.Containers.Indefinite_Holders;
private with Ada.Containers.Indefinite_Vectors;
private with Ada.Containers.Indefinite_Hashed_Maps;

package D_Bus.Types.Containers is
   -------------
   -- Structs --
   -------------
   type Struct (<>) is new Container_Type with private;

   function Empty (Signature : Contents_Signature) return Struct;
   --  Produce an empty, INVALID Struct with signature `Signature`
   --  ALL elements must be set before the time can be used.

   function Get (Container : Struct; Index : Positive) return Root_Type'Class;
   --  Get element `Index` in `Container`
   --  `Index` must be within the bounds of `Container`

   procedure Set
     (Container : out Struct; Index : Positive; Value : Root_Type'Class);
   --  Set element `Index` in `Container` to `Value`
   --  `Index` must be within the bounds of `Container`
   --  `Value` must be the correct type for `Container`

   function Count (Container : Struct) return Positive;
   --  Return the number of elements in `Container`

   overriding function Signature (X : Struct) return Single_Signature;

   overriding function Size
     (X : Struct) return Ada.Streams.Stream_Element_Count;

   overriding function Image (X : Struct) return String;

   overriding function Contents (X : Struct) return Contents_Signature;

   --------------------------------
   -- Iterable Containers Shared --
   --------------------------------
   type Constant_Reference_Type (X : not null access constant Root_Type'Class)
   is
     limited private with
     Implicit_Dereference => X;

   type Reference_Type (X : not null access Root_Type'Class) is
     limited private with
     Implicit_Dereference => X;

     --------------------------
     --       Arrays         --
     --------------------------
   type Array_Cursor (<>) is private;
   No_Index : constant Array_Cursor;

   function Index (Position : Array_Cursor) return Positive;
   --  Get the index of `C`

   function Element (Position : Array_Cursor) return Root_Type'Class;
   --  Retrieve the element at `C`

   function Has_Element (Position : Array_Cursor) return Boolean;

   package Array_Iterators is new Ada.Iterator_Interfaces
     (Array_Cursor, Has_Element);
   subtype Array_Iterator is Array_Iterators.Reversible_Iterator;

   type D_Array (Element_Signature : Interned_Single_Signature) is
     new Container_Type and Array_Iterator with private with
     Constant_Indexing => Constant_Reference_A,
     Variable_Indexing => Reference_A, Default_Iterator => Iterate_A,
     Iterator_Element  => Root_Type'Class;

   function Constant_Reference_A
     (Container : aliased D_Array; Index : Positive)
      return Constant_Reference_Type;

   function Constant_Reference_A
     (Container : aliased D_Array; Position : Array_Cursor)
      return Constant_Reference_Type;

   function Reference_A
     (Container : aliased in out D_Array; Index : Positive)
      return Reference_Type;

   function Reference_A
     (Container : aliased in out D_Array; Position : Array_Cursor)
      return Reference_Type;

   function Iterate_A (Container : D_Array) return Array_Iterator'Class is
     (Container);

   procedure Append (Container : out D_Array; Element : Root_Type'Class);
   --  Append `Element` to `Container`
   --  It must match the `Container`’s signature

   overriding function Signature (X : D_Array) return Single_Signature;

   overriding function Size
     (X : D_Array) return Ada.Streams.Stream_Element_Count;

   overriding function Image (X : D_Array) return String;

   overriding function Contents (X : D_Array) return Contents_Signature;

   ------------------------
   --       Dicts        --
   ------------------------
   type Dict_Cursor (<>) is private;
   No_Key : constant Dict_Cursor;
   function Key (Position : Dict_Cursor) return Basic_Type'Class;
   --  Return the Key for `C`

   function Element (Position : Dict_Cursor) return Root_Type'Class;
   --  Return the Element at `C`

   function Has_Element (Position : Dict_Cursor) return Boolean;

   package Dict_Iterators is new Ada.Iterator_Interfaces
     (Dict_Cursor, Has_Element);
   subtype Dict_Iterator is Dict_Iterators.Forward_Iterator;

   type Dict
     (Key_Signature     : Basic_Signature_Element;
      Element_Signature : Interned_Single_Signature)
   is
     new Container_Type and Dict_Iterator with private with
     Constant_Indexing => Constant_Reference_D,
     Variable_Indexing => Reference_D, Default_Iterator => Iterate_D,
     Iterator_Element  => Root_Type'Class;

   function Constant_Reference_D
     (Container : aliased Dict; Key : Basic_Type'Class)
      return Constant_Reference_Type;

   function Constant_Reference_D
     (Container : aliased Dict; Position : Dict_Cursor)
      return Constant_Reference_Type;

   function Reference_D
     (Container : aliased in out Dict; Key : Basic_Type'Class)
      return Reference_Type;

   function Reference_D
     (Container : aliased in out Dict; Position : Dict_Cursor)
      return Reference_Type;

   function Iterate_D (Container : Dict) return Dict_Iterator'Class is
     (Container);

   procedure Insert
     (Container : in out Dict; Key : Basic_Type'Class;
      Value     :        Root_Type'Class);
   --  Insert <Key, Value> into `Container`
   --  Key must match `Container`’s key type
   --  Value must match `Container`’s element type

   overriding function Signature (X : Dict) return Single_Signature;

   overriding function Size (X : Dict) return Ada.Streams.Stream_Element_Count;

   overriding function Image (X : Dict) return String;

   overriding function Contents (X : Dict) return Contents_Signature;

   --------------
   -- Variants --
   --------------
   type Variant is new Container_Type with private;
   --  Note: MUST be initialised before use or an exception will be raised.

   function "+" (X : Root_Type'Class) return Variant;
   function Get (X : Variant) return Root_Type'Class;
   function "+" (X : Variant) return Root_Type'Class renames Get;

   overriding function Image (X : Variant) return String;
private
   ------------
   -- Shared --
   ------------
   package Root_Type_Holders is new Ada.Containers.Indefinite_Holders
     (Root_Type'Class);

   ------------
   -- Struct --
   ------------
   type Root_Type_Holder_Array is
     array (Positive range <>) of Root_Type_Holders.Holder;
   type Struct (Count : Positive) is new Container_Type with record
      Signatures : Single_Signature_Array (1 .. Count);
      Elements   : Root_Type_Holder_Array (1 .. Count);
   end record;

   procedure Read
     (Stream :     not null access Ada.Streams.Root_Stream_Type'Class;
      Item   : out Struct);

   procedure Write
     (Stream : not null access Ada.Streams.Root_Stream_Type'Class;
      Item   : Struct);

   for Struct'Read use Read;
   for Struct'Write use Write;

   --------------------------
   -- Indexable Containers --
   --------------------------
   type Constant_Reference_Type (X : not null access constant Root_Type'Class)
   is
   limited null record;

   type Reference_Type (X : not null access Root_Type'Class) is
   limited null record;

   ------------
   -- Arrays --
   ------------
   type Array_Cursor is record
      Container : access constant D_Array;
      Index     : Natural;
      --  Note: 0 is an INVALID value
   end record;

   No_Index : constant Array_Cursor := (null, 0);

   package Vectors is new Ada.Containers.Indefinite_Vectors
     (Natural, Root_Type'Class);

   type D_Array (Element_Signature : Interned_Single_Signature) is
   new Container_Type and Array_Iterator with record
      Inner : Vectors.Vector;
   end record;

   procedure Read
     (Stream :     not null access Ada.Streams.Root_Stream_Type'Class;
      Item   : out D_Array);

   procedure Write
     (Stream : not null access Ada.Streams.Root_Stream_Type'Class;
      Item   : D_Array);

   for D_Array'Read use Read;
   for D_Array'Write use Write;

   overriding function First (Object : D_Array) return Array_Cursor;
   overriding function Next
     (Object : D_Array; Position : Array_Cursor) return Array_Cursor;
   overriding function Previous
     (Object : D_Array; Position : Array_Cursor) return Array_Cursor;
   overriding function Last (Object : D_Array) return Array_Cursor;
   -----------
   -- Dicts --
   -----------
   type Dict_Cursor is record
      Container : access constant Dict;
      Key       : access constant Basic_Type'Class;
   end record;

   No_Key : constant Dict_Cursor := (null, null);

   function Hash (Key : Basic_Type'Class) return Ada.Containers.Hash_Type;

   package Hash_Maps is new Ada.Containers.Indefinite_Hashed_Maps
     (Key_Type => Basic_Type'Class, Element_Type => Root_Type'Class,
      Hash     => Hash, Equivalent_Keys => "=");

   type Dict
     (Key_Signature     : Basic_Signature_Element;
      Element_Signature : Interned_Single_Signature)
   is
   new Container_Type and Dict_Iterator with record
      Inner : Hash_Maps.Map;
   end record;

   procedure Read
     (Stream :     not null access Ada.Streams.Root_Stream_Type'Class;
      Item   : out Dict);

   procedure Write
     (Stream : not null access Ada.Streams.Root_Stream_Type'Class;
      Item   : Dict);

   for Dict'Read use Read;
   for Dict'Write use Write;

   overriding function First (Object : Dict) return Dict_Cursor;
   overriding function Next
     (Object : Dict; Position : Dict_Cursor) return Dict_Cursor;

   --------------
   -- Variants --
   --------------
   type Variant is new Container_Type with record
      I : Root_Type_Holders.Holder := raise Initialisation_Required;
   end record;

   overriding function Contents (X : Variant) return Contents_Signature is
     (Contents_Signature (X.I.Element.Signature));

   overriding function Signature (X : Variant) return Single_Signature is
     ("v");

   overriding function Size
     (X : Variant) return Ada.Streams.Stream_Element_Count;

   procedure Read_Variant
     (Stream :     not null access Ada.Streams.Root_Stream_Type'Class;
      Item   : out Variant);

   procedure Write_Variant
     (Stream : not null access Ada.Streams.Root_Stream_Type'Class;
      Item   : Variant);

   for Variant'Read use Read_Variant;
   for Variant'Write use Write_Variant;
end D_Bus.Types.Containers;
