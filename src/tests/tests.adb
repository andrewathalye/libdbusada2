pragma Ada_2022;

with D_Bus.Connection;
with D_Bus.Messages; use D_Bus.Messages;
with D_Bus.Message_Queues;

with Ada.Text_IO; use Ada.Text_IO;

procedure Tests is
   C : aliased D_Bus.Connection.Connection :=
        D_Bus.Connection.Connect;
   M : D_Bus.Messages.Message;
   Q : D_Bus.Message_Queues.Message_Queue;
begin
   Q.Send (Compose_Call
     (Path => "/org/freedesktop/DBus",
      M_Interface => "org.freedesktop.DBus",
      Member => "Hello",
      Destination => "org.freedesktop.DBus"));

   Q.Send (Compose_Call
     (Path => "/org/freedesktop/DBus",
      M_Interface => "org.freedesktop.DBus.Introspectable",
      Member => "Introspect",
      Destination => "org.freedesktop.DBus"));

   Q.Update (C);

   loop
      select
         Q.Receive (M);
      else
         exit;
      end select;

      Put_Line (M_Type (M)'Image);
      begin
         Put_Line (String (Path (M)));
      exception
         when others => null;
      end;
      begin
         Put_Line (String (M_Interface (M)));
      exception
         when others => null;
      end;
      begin
         Put_Line (String (Member (M)));
      exception
         when others => null;
      end;

      for A of Arguments (M) loop
         Put_Line (A.Image);
      end loop;
   end loop;

   D_Bus.Connection.Disconnect (C);
end Tests;
