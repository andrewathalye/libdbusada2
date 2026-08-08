pragma Ada_2012;

with Ada.Text_IO; use Ada.Text_IO;

with D_Bus.Connection; use D_Bus.Connection;
with D_Bus.Messages;   use D_Bus.Messages;
with D_Bus.Types.Basic;
with D_Bus.Logging;
with D_Bus.Types.Containers;
with Interfaces;

procedure Tests is
   subtype SV_Dict is
     D_Bus.Types.Containers.Dict ('s', D_Bus.Types.Intern ("v"));

   C                              : aliased Connection := Connect;
   Session_Handle, Request_Handle : D_Bus.Types.Basic.D_Object_Path;
begin
   D_Bus.Logging.Set_Debug (True);

   Hello : declare
      M : Message;
      R : Message;
   begin
      M :=
        D_Bus.Messages.Compose_Call
          (Path        => "/org/freedesktop/DBus",
           M_Interface => "org.freedesktop.DBus",
           Member      => "Hello",
           Destination => "org.freedesktop.DBus");
      D_Bus.Connection.Send (C, M);

      while Can_Read (C, 0.1) loop
         Receive (C, R);
         for A of Arguments (R) loop
            Put_Line (A.Image);
         end loop;
      end loop;
   end Hello;

   CreateSession : declare
      use D_Bus.Types;
      use D_Bus.Types.Containers;
      use D_Bus.Types.Basic;

      M, R    : Message;
      Options : SV_Dict;
      AL      : Argument_List;
   begin
      M :=
        D_Bus.Messages.Compose_Call
          (Path        => "/org/freedesktop/portal/desktop",
           M_Interface => "org.freedesktop.portal.InputCapture",
           Member      => "CreateSession",
           Destination => "org.freedesktop.portal.Desktop");

      Options.Insert
        (D_String'("session_handle_token"), +D_String'("dbusada"));
      Options.Insert (D_String'("capabilities"), +(Uint32'(1)));

      AL.Append (D_String'("libdbusada2 test program"));
      AL.Append (Options);
      Add_Arguments (M, AL);

      Send (C, M);

      if not Can_Read (C, 0.5) then
         raise Program_Error;
      end if;

      Receive (C, R);
      for A of Arguments (R) loop
         Put_Line (String (A.Signature));
         Put_Line (A.Image);
      end loop;

      Request_Handle := D_Object_Path (Arguments (R).First_Element);
   end CreateSession;

   Await_Response : declare
      use D_Bus.Types;
      use D_Bus.Types.Containers;
      use D_Bus.Types.Basic;
      use type Interfaces.Unsigned_32;

      R        : Message;
      Response : Uint32;
      Results  : SV_Dict;
   begin
      <<Try_Again>>
      if not Can_Read (C, 10.0) then
         raise Program_Error with "Timed out waiting for signal";
      end if;

      Receive (C, R);
      if Path (R) /= +Request_Handle then
         Put_Line ("Response from the wrong object");
         goto Try_Again;
      end if;

      for A of Arguments (R) loop
         Put_Line (String (A.Signature));
         Put_Line (A.Image);
      end loop;

      Response := Uint32 (Arguments (R).First_Element);
      if +Response /= 0 then
         raise Program_Error with "User denied permission";
      end if;

      Results := SV_Dict (Arguments (R).Last_Element);
      Session_Handle :=
        D_Object_Path
          (Variant (Results (D_String'("session_handle")).X.all).Get);
   end Await_Response;

   ConnectToEIS : declare
      use D_Bus.Types;
      use D_Bus.Types.Basic;

      M, R    : Message;
      Options : SV_Dict;
      AL      : Argument_List;
      Result  : File_Descriptor;
   begin
      M :=
        D_Bus.Messages.Compose_Call
          (Path        => "/org/freedesktop/portal/desktop",
           M_Interface => "org.freedesktop.portal.InputCapture",
           Member      => "ConnectToEIS",
           Destination => "org.freedesktop.portal.Desktop");

      AL.Append (Session_Handle);
      AL.Append (Options);
      Add_Arguments (M, AL);
      Send (C, M);

      if not Can_Read (C, 0.5) then
         raise Program_Error;
      end if;

      Receive (C, R);
      for A of Arguments (R) loop
         Put_Line (String (A.Signature));
         Put_Line (A.Image);
      end loop;
   end ConnectToEIS;

   Disconnect (C);
end Tests;
