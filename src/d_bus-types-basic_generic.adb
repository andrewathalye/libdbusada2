pragma Ada_2022;

package body D_Bus.Types.Basic_Generic is
   --------------------
   -- Basic_Wrappers --
   --------------------
   package body Fixed_Wrappers is
      ---------
      -- "+" --
      ---------
      function "+" (X : Inner) return Outer is
      begin
         return (I  => Padded_Inners.Padded_Type (X));
      end "+";

      ---------
      -- "+" --
      ---------
      function "+" (X : Outer) return Inner is
      begin
         return Inner (X.I);
      end "+";

      -----------
      -- Image --
      -----------
      overriding
      function Image (X : Outer) return String is
      begin
         return X.I'Image;
      end Image;
   end Fixed_Wrappers;

   ---------------------
   -- String_Wrappers --
   ---------------------
   package body String_Wrappers is
      ---------
      -- "+" --
      ---------
      function "+" (X : Outer) return External_Type is
      begin
         return External_Type (X.I.Element.S);
      end "+";

      ---------
      -- "+" --
      ---------
      function "+" (X : External_Type) return Outer is
         IT : Internal_Type (X'Length);
      begin
         IT.S := Internal_Raw_String (X);
         return (I => ITH.To_Holder (IT));
      end "+";

      ----------
      -- Size --
      ----------
      function Size (X : Outer) return Ada.Streams.Stream_Element_Count
      is
         use type Ada.Streams.Stream_Element_Offset;
      begin
         return X.I.Element'Size / 8;
      end Size;

      ----------
      -- Read --
      ----------
      procedure Read
        (Stream :     not null access Ada.Streams.Root_Stream_Type'Class;
         Item   : out Outer)
      is
      begin
         Item.I.Replace_Element (Internal_Type'Input (Stream));
      end Read;

      -----------
      -- Write --
      -----------
      procedure Write
        (Stream : not null access Ada.Streams.Root_Stream_Type'Class;
         Item   : Outer)
      is
      begin
         Internal_Type'Output (Stream, Item.I.Element);
      end Write;

      -----------
      -- Image --
      -----------
      function Image (X : Outer) return String is
      begin
         return String (X.I.Element.S);
      end Image;
   end String_Wrappers;
end D_Bus.Types.Basic_Generic;
