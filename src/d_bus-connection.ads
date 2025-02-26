pragma Ada_2012;

with Ada.Environment_Variables;
with Ada.Streams;

with GNAT.Sockets;

with D_Bus.Messages;
private with D_Bus.Types;
private with D_Bus.Streams;

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

   -------------------
   -- Compatibility --
   -------------------
   function FD_Transfer_Support (C : Connected_Connection) return Boolean;
   --  Check whether `C` supports UNIX File Descriptor transfers.
   --  This requires support at both ends of the connection.

   ---------------------
   -- Message Support --
   ---------------------
   procedure Send (C : aliased Connection; M : D_Bus.Messages.Message) with
     Pre => C in Connected_Connection;
   --  Send a message via connection `C`

   procedure Receive
     (C : aliased Connection; M : out D_Bus.Messages.Message) with
     Pre => Connected (C);
   --  Receive a message from connection `C`

   function Check
     (C : Connected_Connection;
      Timeout : GNAT.Sockets.Selector_Duration) return Boolean;
   --  Returns `True` if there is data to be read from `C` and `False`
   --  otherwise.
   --
   --  Wait up to `Timeout` time for data to be available.

   --------------------
   -- Server Address --
   --------------------
   subtype Server_Address is String with
       Dynamic_Predicate => Is_Valid (Server_Address);
   --  A D-Bus Server Address according to the specification.
   --  This may specify multiple concrete servers, the below
   --  subprograms will try them, one by one, and use the first
   --  valid one.

   function Is_Valid (Addr : String) return Boolean;
   --  Validate the format, but not contents, of a server address


   -------------------------------
   -- Standard Server Addresses --
   -------------------------------
   Session_Bus : constant Server_Address;
   --  A standard, OS-neutral address that refers to the user's
   --  session bus, if one exists.

   System_Bus : constant Server_Address;
   --  A standard, OS-neutral address that refers to a systemwide
   --  bus, if one exists.

   ---------------------------
   -- Connection Management --
   ---------------------------
   Address_Error : exception;
   --  An error that occurred while trying to interpret the contents
   --  of an address. This can occur with a well-formed address containing
   --  incompatible keys.

   Transport_Error : exception;
   --  An error that occurred while trying to use (transfer data over)
   --  a server address. This is only propagated if there are no
   --  more servers to try, but each occurrence is logged.

   function Connect
     (Address : Server_Address := Session_Bus) return Connected_Connection;
   --  Connect `C` to one of the servers listed in `Address`
   --  Once connected, you must complete a handshake with the remote server.
   --  TODO

   function Listen (Address : Server_Address) return Connected_Connection;
   --  Start listening on the first valid server address in `Address`
   --  TODO allow listening for multiple connections
   --  TODO in general not sure what we want to do here. full message bus impl?

   procedure Disconnect (C : in out Connected_Connection);
   --  Disconnect and destroy data held by `C`
   --  TODO make object controlled, auto disconnect on leave scope

   type Mode_Type is private;
   --  Implementation detail
private
   Session_Bus : constant Server_Address := "autolaunch:";
   System_Bus  : constant Server_Address :=
     Ada.Environment_Variables.Value
       ("DBUS_SYSTEM_BUS_ADDRESS",
        "unix:path=/var/run/dbus/system_bus_socket");
   --  Note: there is nothing like 'autolaunch' specified for the system bus
     --  this is, IMO, a flaw in the specification, but nevertheless we
     --  need to be standards-compliant.

   type Canonical_Alignable_Stream is
   new D_Bus.Streams.Alignable_Stream with record
      Connection  : not null access constant Connected_Connection;
      Read_Count  : Ada.Streams.Stream_Element_Count := 0;
      Write_Count : Ada.Streams.Stream_Element_Count := 0;
   end record;

   overriding procedure Read_Align
     (Stream    : not null access Canonical_Alignable_Stream;
      Alignment : D_Bus.Streams.Alignment_Type);

   overriding procedure Write_Align
     (Stream    : not null access Canonical_Alignable_Stream;
      Alignment : D_Bus.Streams.Alignment_Type);

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
