pragma Ada_2022;

with D_Bus.Messages;    use D_Bus.Messages;
with D_Bus.Dispatching; use D_Bus.Dispatching;

with Ada.Text_IO; use Ada.Text_IO;
with D_Bus.Types;
with D_Bus.Types.Basic;

procedure Tests is
   type Callback_Obj is record
      Count : Natural := 0;
   end record;

   type NR is null record;

   package CO_Dispatching is new Generic_Dispatching (Callback_Obj);
   package NR_Dispatching is new Generic_Dispatching (NR);

   procedure Dispatcher
     (T : in out Dispatch_Table; O : in out Callback_Obj; M : Message);

   procedure Dispatcher
     (T : in out Dispatch_Table; O : in out Callback_Obj; M : Message)
   is
   begin
      Put_Line ("Dispatcher called!");
      Put_Line (O.Count'Image);
      O.Count := O.Count + 1;

      Send (T, Compose_Return (Reply_To => M, Destination => Sender (M)));
   end Dispatcher;

   T     : D_Bus.Dispatching.Dispatch_Table := D_Bus.Dispatching.Create;
   Reply : D_Bus.Messages.Message;
   M     : D_Bus.Messages.Message;
begin
   Reply :=
     Send
       (T,
        Compose_Call
          (Path        => "/org/freedesktop/DBus",
           M_Interface => "org.freedesktop.DBus", Member => "Hello",
           Destination => "org.freedesktop.DBus"));
   pragma Assert (M_Type (Reply) = Method_Return);

   M :=
     Compose_Call
       (Path => "/org/freedesktop/DBus", M_Interface => "org.freedesktop.DBus",
        Member => "RequestName", Destination => "org.freedesktop.DBus");
   declare
      use D_Bus.Types.Basic;
      Args : D_Bus.Types.Argument_List;
   begin
      Args.Append (D_String'("tk.zenithseeker.test"));
      Args.Append (Uint32'(1));
      Add_Arguments (M, Args);
   end;

   Reply := Send (T, M);
   pragma Assert (M_Type (Reply) = Method_Return);

   CO_Dispatching.Add_Dispatcher
     (T, "tk.zenithseeker.test", Dispatcher'Access);
   CO_Dispatching.Create_Object (T, "/");
   NR_Dispatching.Create_Object (T, "/NR");

   Put_Line ("Ready to Dispatch");

   loop
      Dispatch (T);
   end loop;
end Tests;
