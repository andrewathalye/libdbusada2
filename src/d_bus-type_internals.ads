pragma Ada_2012;

with Ada.Streams;

private with GNATCOLL.Strings;

package D_Bus.Type_Internals is
   ---------------------
   -- Fast Signatures --
   ---------------------
   subtype Single_Signature is String;
   subtype Contents_Signature is String;

   ------------------
   -- Type Classes --
   ------------------
   type Root_Type is interface;
   function Signature (X : Root_Type) return Single_Signature is abstract;

   type Basic_Type is interface and Root_Type;

   type String_Type is interface and Root_Type;
   function Length (X : String_Type) return Natural is abstract;
   function "+" (X : String_Type) return String is abstract;
   function "+" (X : String) return String_Type is abstract;

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
   -- Basic Types --
   -----------------
   generic
      Tag : Single_Signature;
      type Inner is private;
   package Basic_Wrappers is
      type Outer is new Padding and Basic_Type with private;
      function "+" (X : Inner) return Outer;
      function "+" (X : Outer) return Inner;
      function Signature (X : Outer) return Single_Signature is (Tag);
   private
      type Outer is new Padding and Basic_Type with record
         I : Inner;
      end record;
   end Basic_Wrappers;

   ------------------
   -- String Types --
   ------------------
   generic
      Tag : Single_Signature;
      type Data_Length_Type is mod <>;
   package String_Wrappers is
      type Outer is new String_Type with private;

      function Length (X : Outer) return Natural;
      function "+" (X : Outer) return String;
      function "+" (X : String) return Outer;
      function Signature (X : Outer) return Single_Signature is (Tag);
   private
      type Outer is new String_Type with record
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
end D_Bus.Type_Internals;
