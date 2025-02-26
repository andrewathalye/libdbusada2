pragma Ada_2022;

package body D_Bus.Types.Basic_Generic is
   --------------------
   -- Fixed_Wrappers --
   --------------------
   package body Fixed_Wrappers is
      ----------
      -- Read --
      ----------
      procedure Read
        (Stream :     not null access Ada.Streams.Root_Stream_Type'Class;
         Item   : out Outer)
      is
      begin
         D_Bus.Streams.Read_Align (Stream, Inner'Size / 8);
         Inner'Read (Stream, Item.I);
      end Read;

      procedure Write
        (Stream : not null access Ada.Streams.Root_Stream_Type'Class;
         Item   : Outer)
      is
      begin
         D_Bus.Streams.Write_Align (Stream, Inner'Size / 8);
         Inner'Write (Stream, Item.I);
      end Write;

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
      function Size
        (X : Outer; Count : Ada.Streams.Stream_Element_Count)
         return Ada.Streams.Stream_Element_Count
      is
         use type Ada.Streams.Stream_Element_Offset;
      begin
         return
           D_Bus.Streams.Alignment_Bytes (Count, Data_Length_Type'Size / 8) +
           (X.I.Element.L'Size + X.I.Element.S'Size + X.I.Element.C'Size) / 8;
      end Size;

      ----------
      -- Read --
      ----------
      procedure Read
        (Stream :     not null access Ada.Streams.Root_Stream_Type'Class;
         Item   : out Outer)
      is
         use type Ada.Streams.Stream_Element_Offset;
      begin
         D_Bus.Streams.Read_Align (Stream, Data_Length_Type'Size / 8);
         Item.I.Replace_Element (Internal_Type'Input (Stream));
      end Read;

      -----------
      -- Write --
      -----------
      procedure Write
        (Stream : not null access Ada.Streams.Root_Stream_Type'Class;
         Item   : Outer)
      is
         use type Ada.Streams.Stream_Element_Offset;
      begin
         D_Bus.Streams.Write_Align (Stream, Data_Length_Type'Size / 8);
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
