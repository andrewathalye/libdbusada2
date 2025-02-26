pragma Ada_2012;

with Ada.Containers.Synchronized_Queue_Interfaces;
with Ada.Containers.Unbounded_Synchronized_Queues;

with D_Bus.Connection;
with D_Bus.Messages;

package D_Bus.Message_Queues is
   package Message_SQI is new Ada.Containers.Synchronized_Queue_Interfaces
     (D_Bus.Messages.Message);

   package Message_BSQ is new Ada.Containers.Unbounded_Synchronized_Queues
     (Queue_Interfaces => Message_SQI);

   protected type Message_Queue is
      procedure Send (Message : D_Bus.Messages.Message);
      --  Add a message to the send queue of `Queue`

      entry Receive (Message : out D_Bus.Messages.Message);
      --  Pull a message from the receive queue of `Queue`
      --  Blocks if no message is available

      procedure Update (Connection : aliased D_Bus.Connection.Connection);
      --  Update the Queue using `Connection`.
      --  This sends all queued messages and stores all messages waiting
      --  to be received.
   private
      Send_Q, Receive_Q : Message_BSQ.Queue;
   end Message_Queue;
   --  A thread-safe queue for storing and receiving messages
   --  See D_Bus.Connection for more details about thread safety
end D_Bus.Message_Queues;
