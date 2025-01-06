pragma Ada_2012;

with Ada.Streams;
with D_Bus.Messages;
with GNAT.Sockets;

package D_Bus.Connection is
   pragma Assertion_Policy (Dynamic_Predicate => Check);

   ---------------------
   -- Base Operations --
   ---------------------
   type U_Connection is private;
   --  TODO: A connection object is currently not thread-safe
   --  TODO limited?

   function Connected (C : U_Connection) return Boolean;

   subtype Disconnected_Connection is U_Connection
   with Dynamic_Predicate => not Connected (Disconnected_Connection);

   subtype Connected_Connection is U_Connection
   with Dynamic_Predicate => Connected (Connected_Connection);

   procedure Disconnect (C : in out Connected_Connection);
   --  Disconnect and destroy data held by `C`
   --  TODO make object controlled, auto disconnect on leave scope

   --------------------
   -- Stream Support --
   --------------------
   type Alignable_Stream is new Ada.Streams.Root_Stream_Type with private;
   type Alignable_Stream_Access is access all Alignable_Stream;
   --  A stream which supports aligning data reads and writes.

   procedure Reset_Count
     (Stream : not null access Alignable_Stream);
   --  Reset the stream’s read/write statistics.

   function Read_Count
     (Stream : not null access Alignable_Stream)
     return Ada.Streams.Stream_Element_Offset;
   --  Get the current read offset into `Stream`

   function Write_Count
     (Stream : not null access Alignable_Stream)
     return Ada.Streams.Stream_Element_Offset;
   --  Get the current write offset into `Stream`

   ---------------------
   -- Message Support --
   ---------------------
   procedure Send (C : Connected_Connection; M : D_Bus.Messages.Message);
   --  Send a message via connection `C`

   function Receive (C : Connected_Connection) return D_Bus.Messages.Message;
   --  Receive a message from connection `C`

   ----------------
   -- Clientside --
   ----------------
   procedure Connect
     (C : in out Disconnected_Connection;
      Address : String);
   --  Where `Address` conforms to the specification
   --  Return when `C` has connected to `Address`

   ----------------
   -- Serverside --
   ----------------
   procedure Listen
     (C : in out Disconnected_Connection;
      Address : String);
   --  Where `Address` conforms to the specification
   --  Return when a client has connected to `Address`
private
   type Alignable_Stream is new Ada.Streams.Root_Stream_Type with record
      Socket : GNAT.Sockets.Socket_Type;
      Read_Count : Ada.Streams.Stream_Element_Count := 0;
      Write_Count : Ada.Streams.Stream_Element_Count := 0;
   end record;

   overriding procedure Read
     (Stream : in out Alignable_Stream;
      Item   : out Ada.Streams.Stream_Element_Array;
      Last   : out Ada.Streams.Stream_Element_Offset);

   overriding procedure Write
     (Stream : in out Alignable_Stream;
      Item   : Ada.Streams.Stream_Element_Array);

   type U_Connection is record
      Connected : Boolean := False;
      Socket : GNAT.Sockets.Socket_Type;
      Stream : Alignable_Stream_Access;
      Unix_Fd_Support : Boolean := False;
   end record;
end D_Bus.Connection;
