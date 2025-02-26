package body D_Bus.Streams is
   procedure Read_Align
     (Stream    : not null access Possible_Alignable_Stream;
      Alignment : Alignment_Type)
   is
   begin
      Alignable_Stream'Class (Stream.all)'Access.Read_Align (Alignment);
   end Read_Align;

   procedure Write_Align
     (Stream    : not null access Possible_Alignable_Stream;
      Alignment : Alignment_Type)
   is
   begin
      Alignable_Stream'Class (Stream.all)'Access.Write_Align (Alignment);
   end Write_Align;

   function Alignment_Bytes
     (Count : Ada.Streams.Stream_Element_Count; Alignment : Alignment_Type)
      return Ada.Streams.Stream_Element_Count
   is
      use type Ada.Streams.Stream_Element_Count;

      Discrepancy : Ada.Streams.Stream_Element_Offset;
   begin
      Discrepancy := Alignment - (Count mod Alignment);

      if Discrepancy = Alignment then
         Discrepancy := 0;
      end if;

      return Discrepancy;
   end Alignment_Bytes;

end D_Bus.Streams;
