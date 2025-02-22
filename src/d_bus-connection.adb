pragma Ada_2012;

with Ada.IO_Exceptions;
with Ada.Tags;
with Ada.Text_IO;
with Ada.Real_Time;
with Ada.Numerics.Discrete_Random;

with D_Bus.Connection.Parse_Address;
with D_Bus.Connection.Try_Authenticate;

with D_Bus.Platform;
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
   function Stream (C : aliased Connection) return Canonical_Alignable_Stream;
   function Stream (C : aliased Connection) return Canonical_Alignable_Stream
   is
   begin
      return
        (Ada.Streams.Root_Stream_Type with Connection => C'Unrestricted_Access,
         others                                       => <>);
   end Stream;

   --  TODO don’t make a fresh stream?
   procedure Send (C : aliased Connection; M : D_Bus.Messages.Message) is
      S : aliased Canonical_Alignable_Stream := Stream (C);
   begin
      D_Bus.Messages.Message'Write (S'Access, M);
   end Send;

   procedure Receive (C : aliased Connection; M : out D_Bus.Messages.Message)
   is
      S : aliased Canonical_Alignable_Stream := Stream (C);
   begin
      D_Bus.Messages.Message'Read (S'Access, M);
   end Receive;

   -----------------
   -- FD Transfer --
   -----------------
   function FD_Transfer_Support (S : GNAT.Sockets.Socket_Type) return Boolean
   is
      use GNAT.Sockets;
   begin
      return D_Bus.Platform.FD_Transfer_Support
         and Get_Socket_Name (S).Family = Family_Unix;
   end FD_Transfer_Support;

   function FD_Transfer_Support (C : Connected_Connection) return Boolean
   is (C.Unix_Fd_Support and FD_Transfer_Support (C.Socket));

   --------------------
   -- Random Numbers --
   --------------------
   package Character_Random is new Ada.Numerics.Discrete_Random (Character);
   Generator : Character_Random.Generator;
   function New_UUID return D_Bus.Types.UUID is
      Tmp : D_Bus.Types.UUID;
   begin
      for C of Tmp loop
         C := Character_Random.Random (Generator);
      end loop;

      return Tmp;
   end New_UUID;

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
      return True;
      pragma Warnings (Off);
      --  TODO crashing here
      return GNAT.Regpat.Match (Server_Regpat, Addr);
      pragma Warnings (On);
   end Is_Valid;

   function Connect
     (Address : Server_Address := Default_Autolaunch)
      return Connected_Connection
   is
      Set : GNAT.Sockets.Socket_Set_Type :=
        D_Bus.Connection.Parse_Address (Connect, Address);

      Result : aliased Connection;
   begin
      while not GNAT.Sockets.Is_Empty (Set) loop
         GNAT.Sockets.Get (Set, Result.Socket);

         if D_Bus.Connection.Try_Authenticate (Connect, Result) then
            Ada.Text_IO.Put_Line ("Successful client authenticate TODO");
            exit;
         end if;
      end loop;

      --  Close all unneeded sockets
      declare
         Extra : GNAT.Sockets.Socket_Type;
      begin
         while not GNAT.Sockets.Is_Empty (Set) loop
            GNAT.Sockets.Get (Set, Extra);
            GNAT.Sockets.Close_Socket (Extra);
         end loop;
      end;

      return Result;
   end Connect;

   function Listen (Address : Server_Address) return Connected_Connection
   is
      Set                : GNAT.Sockets.Socket_Set_Type :=
        D_Bus.Connection.Parse_Address (Connect, Address);
      Unconnected_Socket : GNAT.Sockets.Socket_Type;
      Client_Addr        : GNAT.Sockets.Sock_Addr_Type;

      Result : aliased Connection;
   begin
      --  TODO we need to redo this
      while not GNAT.Sockets.Is_Empty (Set) loop
         GNAT.Sockets.Get (Set, Unconnected_Socket);
         GNAT.Sockets.Accept_Socket
           (Unconnected_Socket, Result.Socket, Client_Addr);

         Result.UUID := New_UUID;

         Ada.Text_IO.Put_Line
           ("Accept connection from client TODO" &
            GNAT.Sockets.Image (Client_Addr));

         --  No need to separately close Unconnected_Socket?
         --  TODO
         if D_Bus.Connection.Try_Authenticate (Listen, Result) then
            Ada.Text_IO.Put_Line ("Authenticate success TODO");
            exit;
         else
            GNAT.Sockets.Close_Socket (Result.Socket);
         end if;
      end loop;

      --  Close all unneeded sockets
      while not GNAT.Sockets.Is_Empty (Set) loop
         GNAT.Sockets.Get (Set, Unconnected_Socket);
         GNAT.Sockets.Close_Socket (Unconnected_Socket);
      end loop;

      return Result;
   end Listen;

   procedure Disconnect (C : in out Connected_Connection) is
   begin
      GNAT.Sockets.Close_Socket (C.Socket);
      C.Socket := GNAT.Sockets.No_Socket;
   end Disconnect;
begin
   --  Seed random number generator
   declare
      use Ada.Real_Time;
      Seconds : Seconds_Count;
      TS      : Time_Span;

      type Unsigned_Seconds is mod 2**Seconds_Count'Size;
      type Unsigned_Integer is mod 2**Integer'Size / 2;
      Seed : Integer;
   begin
      Split (Clock, Seconds, TS);
      Seed :=
        Integer
          (Unsigned_Seconds (Seconds) and
           Unsigned_Seconds (Unsigned_Integer'Last));
      Character_Random.Reset (Generator, Seed);
   end;
end D_Bus.Connection;
