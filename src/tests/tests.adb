pragma Ada_2012;

with Ada.Text_IO; use Ada.Text_IO;

with D_Bus.Connection; use D_Bus.Connection;
with D_Bus.Messages; use D_Bus.Messages;
with D_Bus.Types.Basic;

procedure Tests is
   C : aliased Connection := Connect;
   M : Message;
begin
   M := D_Bus.Messages.Compose_Call
     (Path => "/org/freedesktop/DBus",
      M_Interface => "org.freedesktop.DBus",
      Member => "Hello",
      Destination => "org.freedesktop.DBus");
   D_Bus.Connection.Send (C, M);

   M := D_Bus.Messages.Compose_Call
     (Path => "/org/freedesktop/DBus",
      M_Interface => "org.freedesktop.DBus",
      Member => "ListNames",
      Destination => "org.freedesktop.DBus");
   D_Bus.Connection.Send (C, M);

   while Can_Read (C, 0.0) loop
      declare
         R : Message;
      begin
         Receive (C, R);
         for A of Arguments (R) loop
            Put_Line (A.Image);
         end loop;
      end;
   end loop;

   Disconnect (C);
end Tests;
