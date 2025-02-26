pragma Ada_2012;

with Ada.Containers;

with GNAT.Sockets;

use type Ada.Containers.Count_Type;

package body D_Bus.Message_Queues is
   protected body Message_Queue is
      pragma Warnings (Off, "potentially blocking");
      procedure Send
        (Message : D_Bus.Messages.Message)
      is
      begin
         Send_Q.Enqueue (Message);
      end Send;

      entry Receive (Message : out D_Bus.Messages.Message)
       when Receive_Q.Current_Use > 0
      is
      begin
         Receive_Q.Dequeue (Message);
      end Receive;

      procedure Update
        (Connection : aliased D_Bus.Connection.Connection)
      is
         use D_Bus.Messages;

         Needs_Reply : Boolean := False;
         Interactive_Auth : Boolean := False;

         Calculated_Timeout : GNAT.Sockets.Selector_Duration :=
           GNAT.Sockets.Immediate;

         M : D_Bus.Messages.Message;
      begin
         --  Send all messages
         while Send_Q.Current_Use > 0 loop
            Send_Q.Dequeue (M);

            if Flags (M).Allow_Interactive_Authentication then
               Interactive_Auth := True;
            end if;

            if not Flags (M).No_Reply_Expected then
               Needs_Reply := True;
            end if;

            D_Bus.Connection.Send (Connection, M);
         end loop;

         --  TODO add these numbers somewhere, check if reasonable?
         if Interactive_Auth then
            Calculated_Timeout := 120.0;
         elsif Needs_Reply then
            Calculated_Timeout := 5.0;
         else
            Calculated_Timeout := GNAT.Sockets.Immediate;
         end if;

         while D_Bus.Connection.Check (Connection, Calculated_Timeout) loop
            --  Prevent Denial-of-Service
            Calculated_Timeout := GNAT.Sockets.Immediate;

            D_Bus.Connection.Receive (Connection, M);
            Receive_Q.Enqueue (M);
         end loop;
      end Update;
      pragma Warnings (On);
   end Message_Queue;
end D_Bus.Message_Queues;
