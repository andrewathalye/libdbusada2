pragma Ada_2022;

with D_Bus.Connection;  use D_Bus.Connection;
with D_Bus.Messages;    use D_Bus.Messages;
with D_Bus.Dispatching; use D_Bus.Dispatching;

with Ada.Text_IO; use Ada.Text_IO;
with D_Bus.Types;
with D_Bus.Types.Basic;

procedure Tests is
   C            : aliased constant D_Bus.Connection.Connection :=
     D_Bus.Connection.Connect;
   Call1, Call2 : D_Bus.Messages.Message;
   Discard      : D_Bus.Messages.Message;
   T            : D_Bus.Dispatching.Dispatch_Table;

   type Callback_Obj is record
      Count : Natural := 0;
   end record;
   package CO_Dispatching is new Generic_Dispatching (Callback_Obj);

   function Handler
     (C : aliased D_Bus.Connection.Connection; O : in out Callback_Obj;
      M :         Message) return Message;
   function Handler
     (C : aliased D_Bus.Connection.Connection; O : in out Callback_Obj;
      M :         Message) return Message
   is
      pragma Unreferenced (C);
   begin
      Put_Line ("Handler called!");
      Put_Line (O.Count'Image);
      O.Count := O.Count + 1;

      return Compose_Return (Reply_To => M, Destination => Sender (M));
   end Handler;
begin
   Call1 :=
     Compose_Call
       (Path => "/org/freedesktop/DBus", M_Interface => "org.freedesktop.DBus",
        Member => "Hello", Destination => "org.freedesktop.DBus");

   Call2 :=
     Compose_Call
       (Path => "/org/freedesktop/DBus", M_Interface => "org.freedesktop.DBus",
        Member => "RequestName", Destination => "org.freedesktop.DBus");

   declare
      Str  : D_Bus.Types.Basic.D_String;
      U32  : D_Bus.Types.Basic.Uint32;
      Args : D_Bus.Types.Argument_List;
   begin
      Str := "tk.zenithseeker.test";
      U32 := 1;
      Args.Append (Str);
      Args.Append (U32);
      Add_Arguments (Call2, Args);
   end;

   Send (C, Call1);
   Send (C, Call2);

   Discard := Get_Reply (T, C, Call1);
   Discard := Get_Reply (T, C, Call2);

   CO_Dispatching.Add_Dispatcher (T, "tk.zenithseeker.test", Handler'Access);
   CO_Dispatching.Create_Object (T, "/");

   Put_Line ("Dispatch");

   loop
      Dispatch (T, C);
   end loop;

   --   D_Bus.Connection.Disconnect (C);
end Tests;
