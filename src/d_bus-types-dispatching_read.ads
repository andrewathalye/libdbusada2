with Ada.Streams;

function D_Bus.Types.Dispatching_Read
     (Stream : not null access Ada.Streams.Root_Stream_Type'Class;
      Signature : Single_Signature) return Root_Type'Class;
   --  Read one single complete type from the stream
