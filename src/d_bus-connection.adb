pragma Ada_2012;

with Ada.IO_Exceptions;
with Ada.Real_Time;
with Ada.Numerics.Discrete_Random;

with D_Bus.Connection.Parse_Address;
with D_Bus.Connection.Try_Authenticate;
with D_Bus.Platform;
with D_Bus.Logging; use D_Bus.Logging;

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
   procedure Read_Align
     (Stream    : not null access Canonical_Alignable_Stream;
      Alignment : D_Bus.Streams.Alignment_Type)
   is
      Discard : Character;
   begin
      for I in
        1 .. D_Bus.Streams.Alignment_Bytes (Stream.Read_Count, Alignment)
      loop
         Character'Read (Stream, Discard);
      end loop;
   end Read_Align;

   procedure Write_Align
     (Stream    : not null access Canonical_Alignable_Stream;
      Alignment : D_Bus.Streams.Alignment_Type)
   is
   begin
      for I in
        1 .. D_Bus.Streams.Alignment_Bytes (Stream.Write_Count, Alignment)
      loop
         Character'Write (Stream, ASCII.NUL);
      end loop;
   end Write_Align;

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
   procedure Send (C : aliased Connection; M : in out D_Bus.Messages.Message)
   is
      S : aliased Canonical_Alignable_Stream := Stream (C);
   begin
      D_Bus.Messages.Write (S'Access, M);
   end Send;

   procedure Receive (C : aliased Connection; M : out D_Bus.Messages.Message)
   is
      S : aliased Canonical_Alignable_Stream := Stream (C);
   begin
      D_Bus.Messages.Read (S'Access, M);
   end Receive;

   function Check
     (C : Connected_Connection; Timeout : GNAT.Sockets.Selector_Duration)
      return Boolean
   is
      use type GNAT.Sockets.Selector_Status;

      R_Set, W_Set : GNAT.Sockets.Socket_Set_Type;
      Status       : GNAT.Sockets.Selector_Status;
   begin
      GNAT.Sockets.Set (R_Set, C.Socket);
      GNAT.Sockets.Check_Selector
        (Selector     => GNAT.Sockets.Null_Selector, R_Socket_Set => R_Set,
         W_Socket_Set => W_Set, Status => Status, Timeout => Timeout);

      return Status = GNAT.Sockets.Completed;
   end Check;

   -----------------
   -- FD Transfer --
   -----------------
   function FD_Transfer_Support (C : Connected_Connection) return Boolean is
     (C.Unix_Fd_Support and D_Bus.Platform.FD_Transfer_Support (C.Socket));

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
   function Connect
     (Address : D_Bus.Types.Extra.Server_Address := Session_Bus)
      return Connected_Connection
   is
      Set : GNAT.Sockets.Socket_Set_Type :=
        D_Bus.Connection.Parse_Address (Connect, Address);

      Result : aliased Connection;
   begin
      while not GNAT.Sockets.Is_Empty (Set) loop
         GNAT.Sockets.Get (Set, Result.Socket);

         if D_Bus.Connection.Try_Authenticate (Connect, Result) then
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

   function Listen
     (Address : D_Bus.Types.Extra.Server_Address) return Connected_Connection
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

         Log
           (Warning,
            ("Accept connection from client TODO" &
             GNAT.Sockets.Image (Client_Addr)));

         --  No need to separately close Unconnected_Socket?
         --  TODO
         if D_Bus.Connection.Try_Authenticate (Listen, Result) then
            Log (Warning, "Authenticate success TODO");
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
