pragma Ada_2012;

with Ada.Streams;
with Ada.Containers.Indefinite_Doubly_Linked_Lists;

package D_Bus.Types is
   pragma Assertion_Policy (Dynamic_Predicate => Check);
   pragma Assertion_Policy (Static_Predicate => Check);

   ----------
   -- UUID --
   ----------
   type UUID is array (1 .. 16) of Character;
   --  A raw, 16-byte UUID according to the
   --  D-Bus specification. This is _not_
   --  a string, it must be hex-encoded to use as
   --  such.

   --------------------------
   -- Unchecked Signatures --
   --------------------------
   subtype Signature_Element is Character;
   type U_Single_Signature is new String;
   type U_Contents_Signature is new String;

   -------------------------------
   -- Exhaustive Type Code List --
   -------------------------------
   Byte_CC            : constant Signature_Element;
   Boolean_CC         : constant Signature_Element;
   Int16_CC           : constant Signature_Element;
   Uint16_CC          : constant Signature_Element;
   Int32_CC           : constant Signature_Element;
   Uint32_CC          : constant Signature_Element;
   Int64_CC           : constant Signature_Element;
   Uint64_CC          : constant Signature_Element;
   Double_CC          : constant Signature_Element;
   File_Descriptor_CC : constant Signature_Element;

   String_CC      : constant Signature_Element;
   Object_Path_CC : constant Signature_Element;
   Signature_CC   : constant Signature_Element;

   Variant_CC      : constant Signature_Element;
   Struct_Start_CC : constant Signature_Element;
   Struct_End_CC   : constant Signature_Element;
   Dict_Start_CC   : constant Signature_Element;
   Dict_End_CC     : constant Signature_Element;
   Array_CC        : constant Signature_Element;

   --------------------------------
   -- Checked Signature Elements --
   --------------------------------
   subtype Basic_Signature_Element is Signature_Element with
       Static_Predicate =>
        Basic_Signature_Element in
          Byte_CC | Boolean_CC | Int16_CC | Uint16_CC | Int32_CC | Uint32_CC
          | Int64_CC | Uint64_CC | Double_CC | File_Descriptor_CC | String_CC
          | Object_Path_CC | Signature_CC;

   ------------------------
   -- Checked Signatures --
   ------------------------
   function Validate_Single_Signature (X : U_Single_Signature) return Boolean;
   subtype Single_Signature is U_Single_Signature with
       Dynamic_Predicate => Validate_Single_Signature (Single_Signature),
       Predicate_Failure =>
        "Invalid single signature " & String (Single_Signature);
   --  Note: This does NOT check container type nesting.
   --  This omission is out of consideration for performance.
   --  All other specified signature rules are checked.

   function Validate_Contents_Signature
     (X : U_Contents_Signature) return Boolean;
   subtype Contents_Signature is U_Contents_Signature with
       Dynamic_Predicate => Validate_Contents_Signature (Contents_Signature),
       Predicate_Failure =>
        "Invalid contents signature " & String (Contents_Signature);

   --  A type which contains a list of valid `Single_Signature`s

   -------------------------
   -- Interned Signatures --
   -------------------------
   type Interned_Single_Signature is not null access constant Single_Signature;
   for Interned_Single_Signature'Storage_Size use 0;
   type Interned_Contents_Signature is
     not null access constant Contents_Signature;
   for Interned_Contents_Signature'Storage_Size use 0;
   --  Type signatures are cached during runtime
   --  These types must only be allocated via `Intern`

   function Intern (X : Single_Signature) return Interned_Single_Signature;
   function Intern (X : Contents_Signature) return Interned_Contents_Signature;
   --  These functions will always return the same value for a signature `X`

   --------------------------
   -- Arrays of Signatures --
   --------------------------
   type Single_Signature_Array is
     array (Positive range <>) of Interned_Single_Signature with
     Dynamic_Predicate => Single_Signature_Array'Length > 0;

   function Split_Signature
     (X : Contents_Signature) return Single_Signature_Array;
   --  Split a Contents_Signature into its constituent Single_Signatures

   ------------------
   -- Type Classes --
   ------------------
   type Root_Type is interface;

   function Signature (X : Root_Type) return Single_Signature is abstract;
   --  Signature of `X` as a single element

   function Size
     (X : Root_Type; Count : Ada.Streams.Stream_Element_Count)
      return Ada.Streams.Stream_Element_Count is abstract;
   --  Size in bytes of `X` including padding at offset `Count`
   --  Call with Count = 0 to see the size without external padding.
   --  This is the exact number of bytes it would take to write or
   --  read `X` from a stream.

   function Image (X : Root_Type) return String is abstract;
   --  Retrieve a view of `X` as a String
   --  For debugging and internal comparison

   function "=" (L, R : Root_Type'Class) return Boolean;
   --  This comparison operator is SLOW and should be avoided

   type Basic_Type is interface and Root_Type;

   type Container_Type is interface and Root_Type;
   function Contents
     (X : Container_Type) return Contents_Signature is abstract;
   --  Signature of the contents of Container `X`

   --------------------
   -- Argument Lists --
   --------------------
   package Argument_Lists is new Ada.Containers.Indefinite_Doubly_Linked_Lists
     (Root_Type'Class);
   subtype Argument_List is Argument_Lists.List;
   --  A list of arbitrary D-Bus types

   function Signature (X : Argument_List) return Contents_Signature;
   --  Signature of `X` as a list of elements
   --  `X` must NOT be empty, since a signature type cannot be empty.

   function Size (X : Argument_List) return Ada.Streams.Stream_Element_Count;
   --  Size in bytes of the arguments if serialised. This includes all
   --  INTERNAL padding.

   -------------
   -- Padding --
   -------------
   subtype Padding_Alignment is Ada.Streams.Stream_Element_Count range 1 .. 8;
   --  Valid range for padding alignment of a D-Bus standard type.

   function Alignment_For (CC : Signature_Element) return Padding_Alignment;
   pragma Pure_Function (Alignment_For);
   --  Return the alignment required for the given signature element, where
   --  `CC` is able to start a complete type (ex. excludes Dict_Start_CC)

   function Alignment_For (X : Single_Signature) return Padding_Alignment is
     (Alignment_For (X (X'First)));
   pragma Pure_Function (Alignment_For);
   --  Return the alignment required for an object with the given signature.
private
   -------------------------
   -- Constant Completion --
   -------------------------
   Byte_CC            : constant Signature_Element := 'y';
   Boolean_CC         : constant Signature_Element := 'b';
   Int16_CC           : constant Signature_Element := 'n';
   Uint16_CC          : constant Signature_Element := 'q';
   Int32_CC           : constant Signature_Element := 'i';
   Uint32_CC          : constant Signature_Element := 'u';
   Int64_CC           : constant Signature_Element := 'x';
   Uint64_CC          : constant Signature_Element := 't';
   Double_CC          : constant Signature_Element := 'd';
   File_Descriptor_CC : constant Signature_Element := 'h';

   String_CC      : constant Signature_Element := 's';
   Object_Path_CC : constant Signature_Element := 'o';
   Signature_CC   : constant Signature_Element := 'g';

   Variant_CC      : constant Signature_Element := 'v';
   Struct_Start_CC : constant Signature_Element := '(';
   Struct_End_CC   : constant Signature_Element := ')';
   Dict_Start_CC   : constant Signature_Element := '{';
   Dict_End_CC     : constant Signature_Element := '}';
   Array_CC        : constant Signature_Element := 'a';
end D_Bus.Types;
