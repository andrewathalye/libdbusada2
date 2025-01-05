pragma Ada_2012;

with Ada.Streams;

private with GNATCOLL.Strings;
with GNATCOLL.Strings_Impl;

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
   type Unbounded_Single_Signature is private;
   
   type Single_Signature_Array is array (Natural range <>) of
      Unbounded_Single_Signature;

   function To_Unbounded
     (X : Single_Signature) return Unbounded_Single_Signature;
   function To_Fixed
     (X : Unbounded_Single_Signature) return Single_Signature;
   function Split_Signature
     (X : Contents_Signature) return Single_Signature_Array;

   ------------------
   -- Type Classes --
   ------------------
   type Root_Type is interface;
   function Signature (X : Root_Type) return Single_Signature is abstract;
   function "=" (L, R : Root_Type'Class) return Boolean is (False);

   type Basic_Type is interface and Root_Type;
   --  TODO numeric type as well? for arithmetic

   type Container_Type is interface and Root_Type;
   function Contents
     (X : Container_Type) return Contents_Signature is abstract;

   -------------
   -- Padding --
   -------------
   --  Will pad written and read data to the size of
   --  any ancestor type.
   type Padding is abstract tagged null record;
   procedure Read
     (Stream : not null access Ada.Streams.Root_Stream_Type'Class;
      Item : out Padding'Class);
   procedure Write
     (Stream : not null access Ada.Streams.Root_Stream_Type'Class;
      Item : Padding'Class);
   for Padding'Class'Read use Read;
   for Padding'Class'Write use Write;

   -----------------
   -- Fixed Types --
   -----------------
   generic
      Type_Code : Single_Signature;
      type Inner is private;
   package Fixed_Wrappers is
      type Outer is new Padding and Basic_Type with private;

      function "+" (X : Inner) return Outer;
      function "+" (X : Outer) return Inner;

      overriding
      function Signature (X : Outer) return Single_Signature is (Type_Code);
   private
      type Outer is new Padding and Basic_Type with record
         I : Inner;
      end record;
   end Fixed_Wrappers;

   ------------------
   -- String Types --
   ------------------
   generic
      Type_Code : Single_Signature;
      type Data_Length_Type is mod <>;
      type Inner_Type is new String;
   package String_Wrappers is
      type Outer is new Basic_Type with private;

      function "+" (X : Outer) return Inner_Type;
      function "+" (X : Inner_Type) return Outer;

      overriding
      function Signature
        (X : Outer) return Single_Signature is (Type_Code);
   private
      type Outer is new Basic_Type with record
         I : GNATCOLL.Strings.XString;
      end record;

      procedure Read
        (Stream : not null access Ada.Streams.Root_Stream_Type'Class;
         Item : out Outer);

      procedure Write
        (Stream : not null access Ada.Streams.Root_Stream_Type'Class;
         Item : Outer);

      for Outer'Read use Read;
      for Outer'Write use Write;
   end String_Wrappers;

   --  Object Paths
   type U_Object_Path is new String;
   function Validate_Object_Path (X : U_Object_Path) return Boolean;
   subtype Object_Path is U_Object_Path
   with Dynamic_Predicate =>
      Validate_Object_Path (Object_Path);
private
   type Unbounded_Single_Signature is
      new GNATCOLL.Strings.XString with null record;
end D_Bus.Type_Internals;
