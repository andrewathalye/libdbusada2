pragma Ada_2012;

with Ada.IO_Exceptions;
with Ada.Tags;
with Ada.Text_IO;

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
   procedure Send (C : aliased Connection; M : D_Bus.Messages.Message) is
      Stream : aliased Canonical_Alignable_Stream :=
        (Ada.Streams.Root_Stream_Type with Connection => C'Unrestricted_Access,
         others                                       => <>);
   begin
      D_Bus.Messages.Message'Write (Stream'Access, M);
   end Send;

   procedure Receive (C : aliased Connection; M : out D_Bus.Messages.Message)
   is
      Stream : aliased Canonical_Alignable_Stream :=
        (Ada.Streams.Root_Stream_Type with Connection => C'Unrestricted_Access,
         others                                       => <>);
   begin
      D_Bus.Messages.Message'Read (Stream'Access, M);
   end Receive;

   ---------------------------------
   -- Connection Public Interface --
   ---------------------------------
   pragma Style_Checks (Off);
   Server_Regpat : constant GNAT.Regpat.Pattern_Matcher :=
     GNAT.Regpat.Compile
       (Expression =>
          "(([a-z]+):(?:([a-z]+)=((?:[-0-9A-Za-z_\/.\*]|(?:%[0-9A-Fa-f]{2}))+)(?:,([a-z]+)=((?:[-0-9A-Za-z_\/.\*]|(?:%[0-9A-Fa-f]{2}))+))*)?)(?:;(([a-z]+):(?:([a-z]+)=((?:[-0-9A-Za-z_\/.\*]|(?:%[0-9A-Fa-f]{2}))+)(?:,([a-z]+)=((?:[-0-9A-Za-z_\/.\*]|(?:%[0-9A-Fa-f]{2}))+))*)?))*");
   --  Note: Generated from specification using tools/regex_serveraddr.sh
   --  This will recognise any SYNTACTICALLY valid server address
   pragma Style_Checks (On);

   function Is_Valid (Addr : String) return Boolean is
   begin
      return GNAT.Regpat.Match (Server_Regpat, Addr);
   end Is_Valid;

   procedure Connect
     (C       : in out Disconnected_Connection;
      Address :        Server_Address := Default_Autolaunch)
   is
      Set  : GNAT.Sockets.Socket_Set_Type := Parse_Address (Connect, Address);
      Temp : GNAT.Sockets.Socket_Type;
   begin
      while not GNAT.Sockets.Is_Empty (Set) loop
         GNAT.Sockets.Get (Set, C.Socket);

         if Try_Authenticate (Connect, C) then
            Ada.Text_IO.Put_Line ("Successful client authenticate TODO");
            exit;
         end if;
      end loop;

      while not GNAT.Sockets.Is_Empty (Set) loop
         GNAT.Sockets.Get (Set, Temp);
         GNAT.Sockets.Close_Socket (Temp);
      end loop;
   end Connect;

   procedure Listen
     (C : in out Disconnected_Connection; Address : Server_Address)
   is
      Set : GNAT.Sockets.Socket_Set_Type := Parse_Address (Connect, Address);
      Unconnected_Socket : GNAT.Sockets.Socket_Type;
      Client_Addr        : GNAT.Sockets.Sock_Addr_Type;
   begin
      while not GNAT.Sockets.Is_Empty (Set) loop
         GNAT.Sockets.Get (Set, Unconnected_Socket);
         GNAT.Sockets.Accept_Socket
           (Unconnected_Socket, C.Socket, Client_Addr);

         Ada.Text_IO.Put_Line
           ("Accept connection from client TODO" &
            GNAT.Sockets.Image (Client_Addr));

         if Try_Authenticate (Listen, C) then
            Ada.Text_IO.Put_Line ("Authenticate success TODO");
            exit;
         end if;
      end loop;

      while not GNAT.Sockets.Is_Empty (Set) loop
         GNAT.Sockets.Get (Set, Unconnected_Socket);
         GNAT.Sockets.Close_Socket (Unconnected_Socket);
      end loop;
   end Listen;

   procedure Disconnect (C : in out Connected_Connection) is
   begin
      GNAT.Sockets.Close_Socket (C.Socket);
      C.Socket := GNAT.Sockets.No_Socket;
   end Disconnect;

end D_Bus.Connection;
