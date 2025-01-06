pragma Ada_2012;

with Ada.Streams;
with Ada.Characters.Handling;

with GNATCOLL.Strings_Impl;

private with Ada.Containers.Indefinite_Holders;

package D_Bus.Type_Internals is
   pragma Assertion_Policy (Dynamic_Predicate => Check);

   --------------------------
   -- Unchecked Signatures --
   --------------------------
   type U_Single_Signature is new String;
   type U_Contents_Signature is new String;

   ------------------------
   -- Checked Signatures --
   ------------------------
   function Validate_Single_Signature (X : U_Single_Signature) return Boolean;
   subtype Single_Signature is U_Single_Signature
   with Dynamic_Predicate =>
      Validate_Single_Signature (Single_Signature);

   function Validate_Contents_Signature
     (X : U_Contents_Signature) return Boolean;
   subtype Contents_Signature is U_Contents_Signature
   with Dynamic_Predicate =>
      Validate_Contents_Signature (Contents_Signature);

   -------------------------
   -- Unbounded Signature --
   -------------------------
   package Unbounded_Single_Signatures is new GNATCOLL.Strings_Impl.Strings
     (SSize            => GNATCOLL.Strings_Impl.Optimal_String_Size,
      Character_Type   => Character,
      Character_String => Single_Signature,
      To_Lower => Ada.Characters.Handling.To_Lower,
      To_Upper => Ada.Characters.Handling.To_Upper);
   subtype Unbounded_Single_Signature is Unbounded_Single_Signatures.XString;

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

   -----------------
   -- Fixed Types --
   -----------------
   --  Note: `Inner` must not be larger than 64 bits
   generic
      Type_Code : Single_Signature;
      type Inner is private;
   package Fixed_Wrappers is
      type Outer is new Basic_Type with private;

      function "+" (X : Inner) return Outer;
      function "+" (X : Outer) return Inner;
   private
      package Padded_Inners is new Padded_Types (Inner, Inner'Size / 8);
      type Outer is new Basic_Type with record
         I : Padded_Inners.Padded_Type;
      end record;

      overriding
      function Signature (X : Outer) return Single_Signature is (Type_Code);

      overriding
      function Size
        (X : Outer) return Ada.Streams.Stream_Element_Count is (Inner'Size);

      overriding
      function Image (X : Outer) return String;
      --  Note: Implemented in body for Ada version compat
   end Fixed_Wrappers;

   ------------------
   -- String Types --
   ------------------
   generic
      Type_Code : Single_Signature;
      type Data_Length_Type is mod <>;
      type External_Type is new String;
   package String_Wrappers is
      type Outer is new Basic_Type with private;

      function "+" (X : Outer) return External_Type;
      function "+" (X : External_Type) return Outer;
   private
      package Padded_Data_Length_Types
      is new Padded_Types (Data_Length_Type, Data_Length_Type'Size / 8);

      type Internal_Raw_String is array
        (Padded_Data_Length_Types.Padded_Type range <>) of Character;
      type Internal_Type (L : Padded_Data_Length_Types.Padded_Type) is record
         S : Internal_Raw_String (1 .. L) := (others => ' ');
         C : Character := ASCII.NUL;
      end record;
      --  TODO check whether calling 'Output and 'Input does what we want
      Empty_Internal_Type : constant Internal_Type :=
        (L => 0, S => <>, C => <>);

      package ITH is new Ada.Containers.Indefinite_Holders (Internal_Type);

      type Outer is new Basic_Type
      with record
         I : ITH.Holder := ITH.To_Holder (Empty_Internal_Type);
      end record;

      procedure Read
        (Stream : not null access Ada.Streams.Root_Stream_Type'Class;
         Item : out Outer);

      procedure Write
        (Stream : not null access Ada.Streams.Root_Stream_Type'Class;
         Item : Outer);

      for Outer'Read use Read;
      for Outer'Write use Write;

      overriding
      function Signature
        (X : Outer) return Single_Signature is (Type_Code);

      overriding
      function Size
        (X : Outer) return Ada.Streams.Stream_Element_Count;

      overriding
      function Image (X : Outer) return String is (String (X.I.Element.S));
   end String_Wrappers;

   --  Object Paths
   type U_Object_Path is new String;
   function Validate_Object_Path (X : U_Object_Path) return Boolean;
   subtype Object_Path is U_Object_Path
   with Dynamic_Predicate =>
      Validate_Object_Path (Object_Path);
end D_Bus.Type_Internals;
