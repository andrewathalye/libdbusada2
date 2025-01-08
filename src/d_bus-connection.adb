pragma Ada_2012;

with Ada.IO_Exceptions;

package body D_Bus.Connection is
   ---------------
   -- Connected --
   ---------------
   function Connected (C : U_Connection) return Boolean is
   begin
      return C.Connected;
   end Connected;

   ----------------
   -- Disconnect --
   ----------------
   procedure Disconnect (C : in out Connected_Connection) is
   begin
      C.Connected := False;
      GNAT.Sockets.Close_Socket (C.Socket);
   end Disconnect;

   -------------
   -- Streams --
   -------------
   procedure Reset_Count (Stream : not null access Alignable_Stream) is
   begin
      Stream.Read_Count := 0;
      Stream.Write_Count := 0;
   end Reset_Count;

   function Read_Count
     (Stream : not null access Alignable_Stream)
      return Ada.Streams.Stream_Element_Offset
   is
   begin
      return Stream.Read_Count;
   end Read_Count;

   function Write_Count
     (Stream : not null access Alignable_Stream)
      return Ada.Streams.Stream_Element_Offset
   is
   begin
      return Stream.Write_Count;
   end Write_Count;

   overriding procedure Read
     (Stream : in out Alignable_Stream;
      Item   :    out Ada.Streams.Stream_Element_Array;
      Last   :    out Ada.Streams.Stream_Element_Offset)
   is
      use type Ada.Streams.Stream_Element_Offset;
   begin
      GNAT.Sockets.Receive_Socket
        (Socket => Stream.Socket,
         Item => Item,
         Last => Last);

      Stream.Read_Count := Stream.Read_Count + Last;
   end Read;

   overriding procedure Write
     (Stream : in out Alignable_Stream;
      Item   :        Ada.Streams.Stream_Element_Array)
   is
      use type Ada.Streams.Stream_Element_Offset;

      Last : Ada.Streams.Stream_Element_Offset;
   begin
      GNAT.Sockets.Send_Socket
        (Socket => Stream.Socket,
         Item => Item,
         Last => Last);

      Stream.Write_Count := Stream.Write_Count + Last;

      if Last /= Item'Length then
         raise Ada.IO_Exceptions.End_Error;
      end if;
   end Write;

   --------------
   -- Messages --
   --------------
   procedure Send (C : Connected_Connection; M : D_Bus.Messages.Message) is
   begin
      Reset_Count (C.Stream);
      D_Bus.Messages.Message'Write (C.Stream, M);
   end Send;

   function Receive (C : Connected_Connection) return D_Bus.Messages.Message is
   begin
      return M : D_Bus.Messages.Message do
         D_Bus.Messages.Message'Read (C.Stream, M);
      end return;
   end Receive;

   pragma Warnings (Off);
   ------------
   -- Client --
   ------------
   procedure Connect (C : in out Disconnected_Connection; Address : String) is
   begin
      raise Program_Error with "Unimplemented procedure Connect";
   end Connect;

   ------------
   -- Server --
   ------------
   procedure Listen (C : in out Disconnected_Connection; Address : String) is
   begin
      raise Program_Error with "Unimplemented procedure Listen";
   end Listen;
   pragma Warnings (On);
end D_Bus.Connection;
