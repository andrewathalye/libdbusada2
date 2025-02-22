pragma Ada_2012;

with Ada.Streams;

with GNAT.Sockets;

with D_Bus.Messages;
private with D_Bus.Types;

package D_Bus.Connection is
   pragma Assertion_Policy (Dynamic_Predicate => Check);
   pragma Assertion_Policy (Pre => Check);

   ---------------------
   -- Base Operations --
   ---------------------
   type Connection is limited private;
   --  Represents a D-Bus connection. This object must
   --  not be copied and TODO is not thread-safe.

   function Connected (C : Connection) return Boolean;

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

   function FD_Transfer_Support (C : Connected_Connection) return Boolean;
   --  Check whether `C` supports UNIX File Descriptor transfers.
   --  This requires support at both ends of the connection.

   ---------------------
   -- Message Support --
   ---------------------
   procedure Send (C : aliased Connection; M : D_Bus.Messages.Message) with
     Pre => Connected (C);
   --  Send a message via connection `C`

   procedure Receive
     (C : aliased Connection; M : out D_Bus.Messages.Message) with
     Pre => Connected (C);
     --  Receive a message from connection `C`

   ---------------------------
   -- Connection Management --
   ---------------------------
   subtype Server_Address is String with
       Dynamic_Predicate => Is_Valid (Server_Address);
   --  A D-Bus Server Address according to the specification.
   --  This may specify multiple concrete servers, the below
   --  subprograms will try them, one by one, and use the first
   --  valid one.

   function Is_Valid (Addr : String) return Boolean;
   --  Validate the format, but not contents, of a server address

   Default_Autolaunch : constant Server_Address;
   --  A default address that will be used if no specific address
   --  is specified for `Connect`. This will typically be the
   --  userÃ¢ÂÂs session bus, if one exists.

   Address_Error : exception;
   --  An error that occurred while trying to interpret the contents
   --  of an address. This can occur with a well-formed address containing
   --  incompatible keys.

   Transport_Error : exception;
   --  An error that occurred while trying to use (transfer data over)
   --  a server address. This is only propagated if there are no
   --  more servers to try, but each occurrence is logged.

   function Connect
     (Address : Server_Address := Default_Autolaunch)
      return Connected_Connection;
   --  Connect `C` to one of the servers listed in `Address`

   function Listen (Address : Server_Address) return Connected_Connection;
   --  Start listening on the first valid server address in `Address`

   procedure Disconnect (C : in out Connected_Connection);
   --  Disconnect and destroy data held by `C`
   --  TODO make object controlled, auto disconnect on leave scope

   type Mode_Type is private;
   --  Implementation detail
private
   Default_Autolaunch : constant Server_Address := "autolaunch:";

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
      UUID            : D_Bus.Types.UUID;
      Unix_Fd_Support : Boolean                  := False;
   end record;

   type Mode_Type is (Connect, Listen);

   ------------------
   -- Random UUIDs --
   ------------------
   function New_UUID return D_Bus.Types.UUID;
   --  Seeded by library-level elaboration
   --  TODO not thread-safe?
end D_Bus.Connection;
