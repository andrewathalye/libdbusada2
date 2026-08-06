pragma Ada_2012;

with Ada.Text_IO; use Ada.Text_IO;

with D_Bus.Connection; use D_Bus.Connection;
with D_Bus.Messages; use D_Bus.Messages;
with D_Bus.Types.Basic;
with D_Bus.Logging;
with D_Bus.Types.Containers;

use D_Bus.Types.Basic;
use type D_Bus.Types.Containers.Variant;

procedure Tests is
   C : aliased Connection := Connect;
   M : Message;
   A : D_Bus.Types.Argument_List;
   Empty_SvDict : D_Bus.Types.Containers.Dict ('s', D_Bus.Types.Intern ("v"));
begin
   D_Bus.Logging.Set_Debug (True);

   M := D_Bus.Messages.Compose_Call
     (Path => "/org/freedesktop/DBus",
      M_Interface => "org.freedesktop.DBus",
      Member => "Hello",
      Destination => "org.freedesktop.DBus");
   D_Bus.Connection.Send (C, M);

   while Can_Read (C, 0.1) loop
      declare
         R : Message;
      begin
         Receive (C, R);
         for A of Arguments (R) loop
            Put_Line (A.Image);
         end loop;
      end;
   end loop;

   M := D_Bus.Messages.Compose_Call
    (Path => "/org/freedesktop/portal/desktop",
     M_Interface => "org.freedesktop.portal.Camera",
     Member => "AccessCamera",
     Destination => "org.freedesktop.portal.Desktop");

--   Empty_SvDict.Insert (D_String'("$"), +D_String'("$"));
   A.Append (Empty_SvDict);

   D_Bus.Messages.Add_Arguments (M, A);

   D_Bus.Connection.Send (C, M);

   loop
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
