pragma Ada_2012;

with Ada.IO_Exceptions;
with Ada.Tags;

with GNAT.Regpat;

package body D_Bus.Connection is
   ---------------
   -- Connected --
   ---------------
   function Connected (C : Connection) return Boolean is
      use type GNAT.Sockets.Socket_Type;
   begin
      return C.Socket /= GNAT.Sockets.No_Socket;
   end Connected;

   -------------
   -- Streams --
   -------------
   function As_Alignable_Stream
     (Stream : not null access Ada.Streams.Root_Stream_Type'Class)
      return Alignable_Stream_Access
   is
   begin
      if Stream.all not in Alignable_Stream'Class then
         raise Not_Alignable_Stream
           with Ada.Tags.Expanded_Name (Stream.all'Tag);
      end if;
      return Alignable_Stream'Class (Stream.all)'Access;
   end As_Alignable_Stream;

   function Read_Count
     (Stream : not null access Canonical_Alignable_Stream)
      return Ada.Streams.Stream_Element_Offset
   is
   begin
      return Stream.Read_Count;
   end Read_Count;

   function Write_Count
     (Stream : not null access Canonical_Alignable_Stream)
      return Ada.Streams.Stream_Element_Offset
   is
   begin
      return Stream.Write_Count;
   end Write_Count;

   overriding procedure Read
     (Stream : in out Canonical_Alignable_Stream;
      Item   :    out Ada.Streams.Stream_Element_Array;
      Last   :    out Ada.Streams.Stream_Element_Offset)
   is
      use type Ada.Streams.Stream_Element_Offset;
   begin
      GNAT.Sockets.Receive_Socket
        (Socket => Stream.Connection.Socket, Item => Item, Last => Last);

      Stream.Read_Count := Stream.Read_Count + Last;
   end Read;

   overriding procedure Write
     (Stream : in out Canonical_Alignable_Stream;
      Item   :        Ada.Streams.Stream_Element_Array)
   is
      use type Ada.Streams.Stream_Element_Offset;

      Last : Ada.Streams.Stream_Element_Offset;
   begin
      GNAT.Sockets.Send_Socket
        (Socket => Stream.Connection.Socket, Item => Item, Last => Last);

      Stream.Write_Count := Stream.Write_Count + Last;

      if Last /= Item'Length then
         raise Ada.IO_Exceptions.End_Error;
      end if;
   end Write;

   --------------
   -- Messages --
   --------------
   procedure Send
     (C : aliased Connection; M : D_Bus.Messages.Message)
   is
      Stream : aliased Canonical_Alignable_Stream :=
        (Ada.Streams.Root_Stream_Type with Connection => C'Unrestricted_Access,
         others                                       => <>);
   begin
      D_Bus.Messages.Message'Write (Stream'Access, M);
   end Send;

   procedure Receive
     (C : aliased Connection;
      M : out D_Bus.Messages.Message)
   is
      Stream : aliased Canonical_Alignable_Stream :=
        (Ada.Streams.Root_Stream_Type with Connection => C'Unrestricted_Access,
         others                                       => <>);
   begin
      D_Bus.Messages.Message'Read (Stream'Access, M);
   end Receive;

   --------------------------
   -- Connection Internals --
   --------------------------

   ---------------------------------
   -- Connection Public Interface --
   ---------------------------------
   pragma Style_Checks (Off);
   Server_Regpat : constant GNAT.Regpat.Pattern_Matcher :=
     GNAT.Regpat.Compile
       (Expression =>
          "([a-z]+:([a-z]+=([-0-9A-Za-z_\/.\*]|(%[0-9A-Fa-f]{2}))+(,[a-z]+=([-0-9A-Za-z_\/.\*]|(%[0-9A-Fa-f]{2}))+)*)?)(;[a-z]+:([a-z]+=([-0-9A-Za-z_\/.\*]|(%[0-9A-Fa-f]{2}))+(,[a-z]+=([-0-9A-Za-z_\/.\*]|(%[0-9A-Fa-f]{2}))+)*)?)*");
   --  Note: completely custom, generated using script in tools/ from
   --  specification
   pragma Style_Checks (On);

   function Is_Valid (Addr : String) return Boolean is
   begin
      return GNAT.Regpat.Match (Server_Regpat, Addr);
   end Is_Valid;

   pragma Warnings (Off);
   procedure Connect
     (C : in out Disconnected_Connection; Address : Server_Address)
   is
   begin
      raise Program_Error with "Unimplemented procedure Connect";
   end Connect;

   procedure Listen
     (C : in out Disconnected_Connection; Address : Server_Address)
   is
   begin
      raise Program_Error with "Unimplemented procedure Listen";
   end Listen;
   pragma Warnings (On);

   procedure Disconnect (C : in out Connected_Connection) is
   begin
      GNAT.Sockets.Close_Socket (C.Socket);
      C.Socket := GNAT.Sockets.No_Socket;
   end Disconnect;

end D_Bus.Connection;
