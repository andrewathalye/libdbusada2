pragma Ada_2022;

with D_Bus.Connection;
with D_Bus.Messages;

with Ada.Text_IO; use Ada.Text_IO;

procedure Tests is
   C : aliased D_Bus.Connection.Connection :=
        D_Bus.Connection.Connect ("autolaunch:");
   M : D_Bus.Messages.Message;
begin
   M := D_Bus.Messages.Compose_Call
     (Path => "/org/freedesktop/DBus",
      M_Interface => "org.freedesktop.DBus.Introspectable",
      Member => "Introspect",
      Destination => "org.freedesktop.DBus");

   D_Bus.Connection.Send (C, M);
   D_Bus.Connection.Receive (C, M);

   Put_Line (M'Image);

   D_Bus.Connection.Disconnect (C);
end Tests;
