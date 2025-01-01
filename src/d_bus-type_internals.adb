pragma Ada_2012;

with D_Bus.Connection;

package body D_Bus.Type_Internals is
   -------------
   -- Padding --
   -------------
   procedure Read
     (Stream :     not null access Ada.Streams.Root_Stream_Type'Class;
      Item   : out Padding'Class)
   is
      pragma Unmodified (Item);
      use D_Bus.Connection;
      ASA : constant Alignable_Stream_Access :=
         Alignable_Stream (Stream.all)'Access;
   begin
      Align (ASA, Item'Size / 8);
      --  TODO check
   end Read;

   procedure Write
     (Stream : not null access Ada.Streams.Root_Stream_Type'Class;
      Item   : Padding'Class)
   is
      use D_Bus.Connection;
      ASA : constant Alignable_Stream_Access :=
         Alignable_Stream (Stream.all)'Access;
   begin
      Align (ASA, Item'Size / 8);
      --  TODO check
   end Write;

   --------------------
   -- Basic_Wrappers --
   --------------------
   package body Basic_Wrappers is
      ---------
      -- "+" --
      ---------
      function "+" (X : Inner) return Outer is
      begin
         return (I => X);
      end "+";

      ---------
      -- "+" --
      ---------
      function "+" (X : Outer) return Inner is
      begin
         return X.I;
      end "+";
   end Basic_Wrappers;

   ---------------------
   -- String_Wrappers --
   ---------------------
   package body String_Wrappers is
      type Padded_Length is new Padding with record
         I : Data_Length_Type;
      end record;

      ------------
      -- Length --
      ------------
      function Length (X : Outer) return Natural is
      begin
         return GNATCOLL.Strings.Length (X.I);
      end Length;

      ---------
      -- "+" --
      ---------
      function "+" (X : Outer) return String is
      begin
         return GNATCOLL.Strings.To_String (X.I);
      end "+";

      ---------
      -- "+" --
      ---------
      function "+" (X : String) return Outer is
      begin
         return (I => GNATCOLL.Strings.To_XString (X));
      end "+";

      ----------
      -- Read --
      ----------
      procedure Read
        (Stream :     not null access Ada.Streams.Root_Stream_Type'Class;
         Item   : out Outer)
      is
         P : Padded_Length;
      begin
         Padded_Length'Read (Stream, P);

         declare
            Buf : String (1 .. Natural (P.I));
         begin
            String'Read (Stream, Buf);
            Item.I := GNATCOLL.Strings.To_XString (Buf);
         end;
      end Read;

      -----------
      -- Write --
      -----------
      procedure Write
        (Stream : not null access Ada.Streams.Root_Stream_Type'Class;
         Item   : Outer)
      is
      begin
         Padded_Length'Write
           (Stream,
           (I => Data_Length_Type (GNATCOLL.Strings.Length (Item.I))));
         String'Write (Stream, GNATCOLL.Strings.To_String (Item.I));
         Character'Write (Stream, ASCII.NUL);
      end Write;

   end String_Wrappers;

end D_Bus.Type_Internals;
