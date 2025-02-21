pragma Ada_2022;

with Ada.Text_IO; use Ada.Text_IO;

with D_Bus.Types.Basic; use D_Bus.Types.Basic;
with D_Bus.Types.Containers;
use type D_Bus.Types.Containers.Variant;
with D_Bus.Connection;
with D_Bus.Messages;

procedure Tests is
   Arr_V : D_Bus.Types.Containers.D_Array (D_Bus.Types.Intern ("v"));

   Dict_sav : D_Bus.Types.Containers.Dict ('s', D_Bus.Types.Intern ("av"));

   Struct_esav : D_Bus.Types.Containers.Struct :=
     D_Bus.Types.Containers.Empty ("a{sav}");

   Connection : aliased D_Bus.Connection.Connection;
begin
   D_Bus.Connection.Connect (Connection, "unix:path=./outsock");
   D_Bus.Connection.Disconnect (Connection);

   pragma Warnings (Off);
   Put_Line ("Produce Arguments");
   Arr_V.Append (+Byte'(1));

   Dict_sav.Insert (D_String'("Hello"), Arr_V);

   Struct_esav.Set (1, Dict_sav);

   Put_Line ("Open Connection");
   D_Bus.Connection.Connect (Connection, "autolaunch:");
   Stream_Open :
   declare
      Sent, Recvd : D_Bus.Messages.Message;
   begin
      Sent :=
        D_Bus.Messages.Compose_Call
          (Path        => "/com/example/test/Object1",
           M_Interface => "com.example.test.interface", Member => "TestProc",
           Destination => "com.example.test.server");

      D_Bus.Messages.Add_Arguments (Sent, [Struct_esav]);

      D_Bus.Connection.Send (Connection, Sent);

      D_Bus.Connection.Receive (Connection, Recvd);
      Put_Line (Recvd'Image);

      for Arg of D_Bus.Messages.Arguments (Recvd) loop
         Put_Line (Arg.Image);
      end loop;

      Sent := D_Bus.Messages.Compose_Error
        (Flags => (No_Reply_Expected => True, others => <>),
         Error => "org.freedesktop.DBus.Error.Failed",
         Reply_To => Recvd,
         Destination => "com.example.test.client");

      D_Bus.Connection.Send (Connection, Sent);
      D_Bus.Connection.Receive (Connection, Recvd);
      Put_Line (Recvd'Image);

      Put_Line (D_Bus.Messages.Error (Recvd));
   end Stream_Open;
   D_Bus.Connection.Disconnect (Connection);

   --  (a{sav})
   --  ([{Hello.s => [v'1.y]}])
   --  [struct]
   --  ALIGN8 <> 0
   --     [array]
   --     ALIGN4 <> 0
   --     Length 4
   --        [dict]
   --        ALIGN8 <> 4
   --           [string]
   --           ALIGN4 <> 0
   --           Length .5 4
   --           Data .Hello 5
   --           Null_Byte 1
   --
   --           [array] 4 / 6
   --           ALIGN4 <> 2
   --           Length .4 4
   --              [variant] 4
   --                 [signature]
   --                 Length .1 1
   --                 Data .y 1
   --                 Null_Byte 1
   --
   --                 [byte]
   --                 data .1 1
end Tests;
