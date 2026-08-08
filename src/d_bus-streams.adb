package body D_Bus.Streams is
   ---------------
   -- Alignment --
   ---------------
   procedure Read_Align
     (Stream    : not null access Possible_Alignable_Stream;
      Alignment : Alignment_Type) is
   begin
      Alignable_Stream'Class (Stream.all)'Access.Read_Align (Alignment);
   end Read_Align;

   procedure Write_Align
     (Stream    : not null access Possible_Alignable_Stream;
      Alignment : Alignment_Type) is
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

   ----------------------
   -- File Descriptors --
   ----------------------
   function Retrieve_FD
     (Stream : not null access Possible_Alignable_Stream; Index : Natural)
      return GNAT.OS_Lib.File_Descriptor is
   begin
      return Alignable_Stream'Class (Stream.all)'Access.Retrieve_FD (Index);
   end Retrieve_FD;

   function Store_FD
     (Stream : not null access Possible_Alignable_Stream;
      FD     : GNAT.OS_Lib.File_Descriptor) return Natural is
   begin
      return Alignable_Stream'Class (Stream.all)'Access.Store_FD (FD);
   end Store_FD;

   procedure Clear_FDs (Stream : not null access Possible_Alignable_Stream) is
   begin
      Alignable_Stream'Class (Stream.all)'Access.Clear_FDs;
   end Clear_FDs;

   function FD_Count
     (Stream : not null access Possible_Alignable_Stream) return Natural is
   begin
      return Alignable_Stream'Class (Stream.all)'Access.FD_Count;
   end FD_Count;
end D_Bus.Streams;
