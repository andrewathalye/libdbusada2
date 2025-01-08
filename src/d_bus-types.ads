pragma Ada_2012;

with Ada.Streams;
with Ada.Characters.Handling;
with Ada.Containers.Indefinite_Doubly_Linked_Lists;

with GNATCOLL.Strings_Impl;

package D_Bus.Types is
   pragma Assertion_Policy (Dynamic_Predicate => Check);

   --------------------------
   -- Unchecked Signatures --
   --------------------------
   subtype Signature_Element is Character;
   type U_Single_Signature is new String;
   type U_Contents_Signature is new String;

   ------------------------
   -- Checked Signatures --
   ------------------------
   function Validate_Single_Signature (X : U_Single_Signature) return Boolean;
   subtype Single_Signature is U_Single_Signature
   with Dynamic_Predicate =>
      Validate_Single_Signature (Single_Signature);
   --  Note: This does NOT check container type nesting.
   --  This omission is out of consideration for performance.
   --  All other specified signature rules are checked.

   function Validate_Contents_Signature
     (X : U_Contents_Signature) return Boolean;
   subtype Contents_Signature is U_Contents_Signature
   with Dynamic_Predicate =>
      Validate_Contents_Signature (Contents_Signature);
   --  A type which contains a list of valid `Single_Signature`s

   -------------------------
   -- Unbounded Signature --
   -------------------------
   package Unbounded_Single_Signatures is new GNATCOLL.Strings_Impl.Strings
     (SSize            => GNATCOLL.Strings_Impl.Optimal_String_Size,
      Character_Type   => Signature_Element,
      Character_String => U_Single_Signature,
      To_Lower => Ada.Characters.Handling.To_Lower,
      To_Upper => Ada.Characters.Handling.To_Upper);
   subtype Unbounded_Single_Signature is Unbounded_Single_Signatures.XString;
   --  This type does not perform checking!
   --  Convert to `Single_Signature` before use if source is untrusted.

   type Single_Signature_Array is array (Positive range <>)
      of Unbounded_Single_Signature;

   function Split_Signature
     (X : Contents_Signature) return Single_Signature_Array;
   --  Functions for managing arrays of signatures

   ------------------
   -- Type Classes --
   ------------------
   type Root_Type is interface;

   function Signature (X : Root_Type) return Single_Signature is abstract;
   --  Signature of `X` as a single element

   function Size
     (X : Root_Type) return Ada.Streams.Stream_Element_Count is abstract;
   --  Size in bytes of `X` without padding

   function Image (X : Root_Type) return String is abstract;
   --  Retrieve a view of `X` as a String
   --  For debugging and internal comparison

   function "=" (L, R : Root_Type'Class) return Boolean;
   --  This comparison operator is SLOW and should be avoided

   type Basic_Type is interface and Root_Type;
   --  TODO numeric type as well? for arithmetic

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
   function Signature (X : Argument_List) return Contents_Signature;
   function Size (X : Argument_List) return Ada.Streams.Stream_Element_Count;
   --  Size in bytes of the arguments if serialised without padding

   -------------
   -- Padding --
   -------------
   type Padding_Alignment is range 1 .. 8;
   generic
      type Base_Type is private;
      Alignment_Bytes : Padding_Alignment;
   package Padded_Types is
      type Padded_Type is new Base_Type;

      procedure Read
        (Stream : not null access Ada.Streams.Root_Stream_Type'Class;
         Item : out Padded_Type);
      procedure Write
        (Stream : not null access Ada.Streams.Root_Stream_Type'Class;
         Item : Padded_Type);

      for Padded_Type'Read use Read;
      for Padded_Type'Write use Write;
   end Padded_Types;

   -------------------------------
   -- Exhaustive Type Code List --
   -------------------------------
   Byte_CC : constant Signature_Element;
   Boolean_CC : constant Signature_Element;
   Int16_CC : constant Signature_Element;
   Uint16_CC : constant Signature_Element;
   Int32_CC : constant Signature_Element;
   Uint32_CC : constant Signature_Element;
   Int64_CC : constant Signature_Element;
   Uint64_CC : constant Signature_Element;
   Double_CC : constant Signature_Element;
   File_Descriptor_CC : constant Signature_Element;

   String_CC : constant Signature_Element;
   Object_Path_CC : constant Signature_Element;
   Signature_CC : constant Signature_Element;

   Variant_CC : constant Signature_Element;
   Struct_CC : constant Signature_Element;
   Dict_CC : constant Signature_Element;
   Array_CC : constant Signature_Element;

   ------------------
   -- Object Paths --
   ------------------
   type U_Object_Path is new String;
   function Validate_Object_Path (X : U_Object_Path) return Boolean;
   subtype Object_Path is U_Object_Path
   with Dynamic_Predicate =>
      Validate_Object_Path (Object_Path);
private
   --  Constant Completion
   Byte_CC : constant Signature_Element := 'y';
   Boolean_CC : constant Signature_Element := 'b';
   Int16_CC : constant Signature_Element := 'n';
   Uint16_CC : constant Signature_Element := 'q';
   Int32_CC : constant Signature_Element := 'i';
   Uint32_CC : constant Signature_Element := 'u';
   Int64_CC : constant Signature_Element := 'x';
   Uint64_CC : constant Signature_Element := 't';
   Double_CC : constant Signature_Element := 'd';
   File_Descriptor_CC : constant Signature_Element := 'h';

   String_CC : constant Signature_Element := 's';
   Object_Path_CC : constant Signature_Element := 'o';
   Signature_CC : constant Signature_Element := 'g';

   Variant_CC : constant Signature_Element := 'v';
   Struct_CC : constant Signature_Element := '(';
   Dict_CC : constant Signature_Element := '{';
   Array_CC : constant Signature_Element := 'a';
end D_Bus.Types;
