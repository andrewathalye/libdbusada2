pragma Ada_2012;

with Ada.Streams;

package D_Bus.Streams is
   pragma Assertion_Policy (Dynamic_Predicate => Check);

   subtype Alignment_Type is
     Ada.Streams
       .Stream_Element_Count range 1 .. Ada.Streams.Stream_Element_Count'Last;

   type Alignable_Stream is
   abstract new Ada.Streams.Root_Stream_Type with null record;
   --  A stream which supports aligning data reads and writes.
   --  The actual method of alignment is specified by descendent types.

   subtype Possible_Alignable_Stream is Ada.Streams.Root_Stream_Type'Class with
       Dynamic_Predicate =>
        Possible_Alignable_Stream in Alignable_Stream'Class;
   --  A stream which cannot be statically guaranteed to be a descendent
   --  type of Alignable_Stream'Class;

   procedure Read_Align
     (Stream    : not null access Possible_Alignable_Stream;
      Alignment : Alignment_Type);
   procedure Read_Align
     (Stream    : not null access Alignable_Stream;
      Alignment : Alignment_Type) is abstract;
   --  Align `Stream` to `Alignment` by discarding input bytes.

   procedure Write_Align
     (Stream    : not null access Possible_Alignable_Stream;
      Alignment : Alignment_Type);
   procedure Write_Align
     (Stream    : not null access Alignable_Stream;
      Alignment : Alignment_Type) is abstract;
   --  Align `Stream` to `Alignment` by emitting null bytes.

   function Alignment_Bytes
     (Count : Ada.Streams.Stream_Element_Count; Alignment : Alignment_Type)
      return Ada.Streams.Stream_Element_Count;
   --  Return the number of alignment bytes necessary to satisfy the alignment
   --  constraint `Alignment` starting from offset `Count` of an ideal stream.
   --
   --  `Count` = 0 satisfies any alignment constraint
end D_Bus.Streams;
