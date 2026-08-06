pragma Ada_2012;

with Ada.Streams;
with GNAT.OS_Lib;
with Interfaces;

package D_Bus.Streams is
   pragma Assertion_Policy (Dynamic_Predicate => Check);

   -----------------
   --  ALIGNMENT  --
   -----------------
   subtype Alignment_Type is
     Ada.Streams.Stream_Element_Count
       range 1 .. Ada.Streams.Stream_Element_Count'Last;

   type Alignable_Stream is abstract new Ada.Streams.Root_Stream_Type
   with null record;
   --  A stream which supports aligning data reads and writes.
   --  The actual method of alignment is specified by descendent types.

   subtype Possible_Alignable_Stream is Ada.Streams.Root_Stream_Type'Class
   with
     Dynamic_Predicate => Possible_Alignable_Stream in Alignable_Stream'Class;
   --  A stream which cannot be statically guaranteed to be a descendent
   --  type of Alignable_Stream'Class;

   procedure Read_Align
     (Stream    : not null access Possible_Alignable_Stream;
      Alignment : Alignment_Type);
   procedure Read_Align
     (Stream : not null access Alignable_Stream; Alignment : Alignment_Type)
   is abstract;
   --  Align `Stream` to `Alignment` by discarding input bytes.

   procedure Write_Align
     (Stream    : not null access Possible_Alignable_Stream;
      Alignment : Alignment_Type);
   procedure Write_Align
     (Stream : not null access Alignable_Stream; Alignment : Alignment_Type)
   is abstract;
   --  Align `Stream` to `Alignment` by emitting null bytes.

   function Alignment_Bytes
     (Count : Ada.Streams.Stream_Element_Count; Alignment : Alignment_Type)
      return Ada.Streams.Stream_Element_Count;
   --  Return the number of alignment bytes necessary to satisfy the alignment
   --  constraint `Alignment` starting from offset `Count` of an ideal stream.
   --
   --  `Count` = 0 satisfies any alignment constraint

   ------------------------
   --  FILE DESCRIPTORS  --
   ------------------------
   function Retrieve_FD
     (Stream : not null access Possible_Alignable_Stream;
      Index  : Interfaces.Unsigned_32) return GNAT.OS_Lib.File_Descriptor;
   function Retrieve_FD
     (Stream : not null access Alignable_Stream;
      Index  : Interfaces.Unsigned_32) return GNAT.OS_Lib.File_Descriptor is abstract;

   --  Retrieve a FD previously stored in the Stream object
   --  Raise FD_Slot_Empty if there is no FD with that index

   FD_Slot_Empty : exception;

   function Store_FD
     (Stream : not null access Possible_Alignable_Stream;
      FD     : GNAT.OS_Lib.File_Descriptor) return Interfaces.Unsigned_32;
   function Store_FD
     (Stream : not null access Alignable_Stream;
      FD     : GNAT.OS_Lib.File_Descriptor) return Interfaces.Unsigned_32 is abstract;
   --  Store a FD in the Stream object
   --  This returns the index in the FD queue that it is stored at

   procedure Clear_FDs (Stream : not null access Possible_Alignable_Stream);
   procedure Clear_FDs (Stream : not null access Alignable_Stream) is abstract;
   --  Clear any FDs stored in the Stream

   procedure Read_FDs (Stream : not null access Possible_Alignable_Stream);
   procedure Read_FDs (Stream : not null access Alignable_Stream) is abstract;
   --  Call into platform-specific code and read FDs from a Stream

   procedure Write_FDs (Stream : not null access Possible_Alignable_Stream);
   procedure Write_FDs (Stream : not null access Alignable_Stream) is abstract;
   --  Call into platform-specific code and write FDs to a Stream

   function FD_Count
     (Stream : not null access Possible_Alignable_Stream)
      return Interfaces.Unsigned_32;
   function FD_Count
     (Stream : not null access Alignable_Stream)
      return Interfaces.Unsigned_32 is abstract;
   --  Return the number of FDs stored in the stream
end D_Bus.Streams;
