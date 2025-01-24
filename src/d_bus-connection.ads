pragma Ada_2012;

with Ada.Streams;
with D_Bus.Messages;
with GNAT.Sockets;

package D_Bus.Connection is
   pragma Assertion_Policy (Dynamic_Predicate => Check);
   pragma Assertion_Policy (Pre => Check);

   ---------------------
   -- Base Operations --
   ---------------------
   type Connection is private;
   --  TODO: A connection object is currently not thread-safe
   --  TODO limited?

   function Connected (C : Connection) return Boolean;

   subtype Disconnected_Connection is Connection with
       Dynamic_Predicate => not Connected (Disconnected_Connection);

   subtype Connected_Connection is Connection with
       Dynamic_Predicate => Connected (Connected_Connection);

   --------------------
   -- Stream Support --
   --------------------
   type Alignable_Stream is
   abstract new Ada.Streams.Root_Stream_Type with null record;
   type Alignable_Stream_Access is access all Alignable_Stream'Class;
   --  A stream which supports aligning data reads and writes.

   Not_Alignable_Stream : exception;
   function As_Alignable_Stream
     (Stream : not null access Ada.Streams.Root_Stream_Type'Class)
      return Alignable_Stream_Access;
   --  Safely convert an arbitrary Stream to an Alignable_Stream
   --  iff that Stream was originally an Alignable_Stream
   --  Raise `Not_Alignable_Stream` on exception

   function Read_Count
     (Stream : not null access Alignable_Stream)
      return Ada.Streams.Stream_Element_Offset is abstract;
   --  Get the current read offset into `Stream`

   function Write_Count
     (Stream : not null access Alignable_Stream)
      return Ada.Streams.Stream_Element_Offset is abstract;
   --  Get the current write offset into `Stream`

   ---------------------
   -- Message Support --
   ---------------------
   procedure Send
     (C : aliased Connection; M : D_Bus.Messages.Message)
   with Pre => Connected (C);
   --  Send a message via connection `C`

   procedure Receive
     (C : aliased Connection; M : out D_Bus.Messages.Message)
   with Pre => Connected (C);
   --  Receive a message from connection `C`

   ---------------------------
   -- Connection Management --
   ---------------------------
   subtype Server_Address is String with
       Dynamic_Predicate => Is_Valid (Server_Address);
   --  A D-Bus Server Address according to the specification.

   function Is_Valid (Addr : String) return Boolean;
   --  Validate the format, but not contents, of a server address

   procedure Connect
     (C : in out Disconnected_Connection; Address : Server_Address);
   --  Where `Address` conforms to the specification
   --  Return when `C` has connected to `Address`

   procedure Listen
     (C : in out Disconnected_Connection; Address : Server_Address);
   --  Where `Address` conforms to the specification
   --  Return when a client has connected to `Address`

   procedure Disconnect (C : in out Connected_Connection);
   --  Disconnect and destroy data held by `C`
   --  TODO make object controlled, auto disconnect on leave scope

private
   type Canonical_Alignable_Stream is new Alignable_Stream with record
      Connection  : not null access constant Connected_Connection;
      Read_Count  : Ada.Streams.Stream_Element_Count := 0;
      Write_Count : Ada.Streams.Stream_Element_Count := 0;
   end record;

   overriding function Read_Count
     (Stream : not null access Canonical_Alignable_Stream)
      return Ada.Streams.Stream_Element_Offset;

   overriding function Write_Count
     (Stream : not null access Canonical_Alignable_Stream)
      return Ada.Streams.Stream_Element_Offset;

   overriding procedure Read
     (Stream : in out Canonical_Alignable_Stream;
      Item   :    out Ada.Streams.Stream_Element_Array;
      Last   :    out Ada.Streams.Stream_Element_Offset);

   overriding procedure Write
     (Stream : in out Canonical_Alignable_Stream;
      Item   :        Ada.Streams.Stream_Element_Array);

   type Connection is record
      Socket          : GNAT.Sockets.Socket_Type := GNAT.Sockets.No_Socket;
      Unix_Fd_Support : Boolean                  := False;
   end record;
end D_Bus.Connection;
