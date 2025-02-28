pragma Ada_2012;

with Ada.Unchecked_Conversion;
with Ada.Unchecked_Deallocation;
with D_Bus.Errors;
with System;

with GNAT.Sockets;

with D_Bus.Logging; use D_Bus.Logging;

package body D_Bus.Dispatching is

   -------------------------------
   -- Dispatch Table Management --
   -------------------------------
   function Create
     (Connection : in out D_Bus.Connection.Connection) return Dispatch_Table
   is
   begin
      return Table : aliased Dispatch_Table do
         D_Bus.Connection.Move (Connection, Table.Connection);
      end return;
   end Create;

   function Create return Dispatch_Table is
      Connection : D_Bus.Connection.Connection := D_Bus.Connection.Connect;
   begin
      return Table : aliased Dispatch_Table do
         D_Bus.Connection.Move (Connection, Table.Connection);
      end return;
   end Create;

   procedure Destroy
     (Table : out Dispatch_Table; Connection : out D_Bus.Connection.Connection)
   is
   begin
      D_Bus.Connection.Move (Table.Connection, Connection);
   end Destroy;

   ------------------------
   -- Message Management --
   ------------------------
   procedure Try_Send_Error
     (Table : in out Dispatch_Table; Message : D_Bus.Messages.Message;
      Name  :        String; Description : String := "");
   --  Send an error with data `Description` and identity `Name` to the sender
   --  of `Message` iff it expects a reply.

   procedure Try_Send_Error
     (Table : in out Dispatch_Table; Message : D_Bus.Messages.Message;
      Name  :        String; Description : String := "")
   is
      use D_Bus.Messages;

      E    : D_Bus.Messages.Message;
      List : D_Bus.Types.Argument_List;
   begin
      if not Flags (Message).No_Reply_Expected then
         Log (Info, "Send Error: " & Name & " " & Description);
         E :=
           Compose_Error
             (Error       => Name, Reply_To => Message,
              Destination => Sender (Message));

         if Description'Length > 0 then
            List.Append (D_Bus.Types.Basic.Strings."+" (Description));
            Add_Arguments (E, List);
         end if;

         D_Bus.Connection.Send (Table.Connection, E);
      else
         Log (Info, "Skip Error: " & Name & " " & Description);
      end if;
   end Try_Send_Error;

   procedure Dispatch
     (Table : in out Dispatch_Table; Timeout : GNAT.Sockets.Selector_Duration);
   --  Internal variant of Dispatch with Timeout specification

   procedure Dispatch
     (Table : in out Dispatch_Table; Timeout : GNAT.Sockets.Selector_Duration)
   is
      use D_Bus.Messages;

      M : D_Bus.Messages.Message;

   begin
      Log (Info, "Dispatch called. Wait for data.");

      --  Get all available messages
      --  Wait at most `Timeout`
      if D_Bus.Connection.Can_Read (Table.Connection, Timeout) then
         while D_Bus.Connection.Can_Read (Table.Connection, 0.0) loop
            D_Bus.Connection.Receive (Table.Connection, M);
            Table.Messages.Append (M);
         end loop;
      end if;

      --  Iterate over list without using cursors
      while not Table.Messages.Is_Empty loop
         --  It is permissible to recursively call into Dispatch.
         --  Reserve the first message as ours
         M := Table.Messages.First_Element;
         Table.Messages.Delete_First;

         --  Send any replies to a different queue
         --  Handle method calls and signals
         case M_Type (M) is
            when Method_Call =>
               if Table.Objects.Contains (Path (M)) then
                  if Table.Dispatchers.Contains (M_Interface (M)) then
                     declare
                        O_R : constant Object_Record_SP.Ref :=
                          Table.Objects (Path (M));
                        D_R : constant Dispatcher_Record    :=
                          Table.Dispatchers (M_Interface (M));
                     begin
                        Log
                          (Info,
                           "Call Dispatcher: " & String (M_Interface (M)) &
                           " @ " & String (Path (M)));
                        D_R.Wrapper (Table, D_R.Actual, O_R.Get, M);
                     end;
                  else
                     Log (Info, "No Dispatcher: " & String (M_Interface (M)));
                     Try_Send_Error
                       (Table, M, D_Bus.Errors.Unknown_Interface,
                        String (M_Interface (M)));
                  end if;
               else
                  Log (Info, "No Object: " & String (Path (M)));
                  Try_Send_Error
                    (Table, M, D_Bus.Errors.Unknown_Object, String (Path (M)));
               end if;
            when Signal =>
               Table.Signals.Append (M);
               Log (Error, "TODO we don’t handle signals yet");
            when Method_Return | Error =>
               Table.Replies.Append (M);
            when Invalid =>
               raise Protocol_Error;
         end case;
      end loop;
   end Dispatch;

   function Send
     (Table : in out Dispatch_Table; Message : D_Bus.Messages.Message)
      return D_Bus.Messages.Message
   is
      use type Message_Lists.Cursor;

      Sent   : D_Bus.Messages.Message := Message;
      Cursor : Message_Lists.Cursor   := Message_Lists.No_Element;
   begin
      Log (Info, "Send message and await reply");

      --  TODO does this create race conditions?
      --  probably, if another random message arrives
      Dispatch (Table, 0.0);
      D_Bus.Connection.Send (Table.Connection, Sent);

      --  Update table
      --  Interactive authentication messages must be given longer
      if D_Bus.Messages.Flags (Sent).Allow_Interactive_Authentication then
         Dispatch (Table, 120.0);
      else
         Dispatch (Table, 2.0);
      end if;

      --  Try to find a reply
      for C in Table.Replies.Iterate loop
         if D_Bus.Messages.Is_Reply (Sent, Message_Lists.Element (C)) then
            Cursor := C;
            exit;
         end if;
      end loop;

      --  Error out if no reply
      if Cursor = Message_Lists.No_Element then
         raise No_Reply;
      end if;

      --  Remove reply from queue and return
      return
        M : constant D_Bus.Messages.Message := Message_Lists.Element (Cursor)
      do
         Table.Replies.Delete (Cursor);
      end return;
   end Send;

   procedure Send
     (Table : in out Dispatch_Table; Message : D_Bus.Messages.Message)
   is
      Sent : D_Bus.Messages.Message := Message;
   begin
      Log (Info, "Send message without waiting for reply");
      D_Bus.Connection.Send (Table.Connection, Sent);
   end Send;

   procedure Dispatch (Table : in out Dispatch_Table) is
   begin
      Dispatch (Table, GNAT.Sockets.Forever);
   end Dispatch;

   --------------------------------------
   -- Concrete Objects and Dispatchers --
   --------------------------------------
   procedure Destroy_Object
     (Table : in out Dispatch_Table; Path : D_Bus.Types.Basic.Object_Path)
   is
   begin
      if Table.Objects.Contains (Path) then
         Table.Objects.Delete (Path);
      else
         raise No_Object with String (Path);
      end if;
   end Destroy_Object;

   procedure Remove_Dispatcher
     (Table       : in out Dispatch_Table;
      M_Interface :        D_Bus.Types.Extra.Interface_Name)
   is
      C : Dispatcher_Interface_Maps.Cursor;
   begin
      if Table.Dispatchers.Contains (M_Interface) then
         C := Table.Dispatchers.Find (M_Interface);
         Table.Dispatchers.Delete (C);
      else
         raise No_Dispatcher with String (M_Interface);
      end if;
   end Remove_Dispatcher;

   ---------------------
   -- Generic Helpers --
   ---------------------
   function As_Dispatch_Transformer is new Ada.Unchecked_Conversion
     (System.Address, Dispatch_Transformer);
   function As_Release_Function is new Ada.Unchecked_Conversion
     (System.Address, Release_Function);

   procedure Release (Self : in out Object_Record) is
   begin
      Self.Release (Self.Alias);
   end Release;

   -------------------------
   -- Generic_Dispatching --
   -------------------------
   package body Generic_Dispatching is
      type Object_Access is access all Object_Type;
      type Tag_Check is tagged null record;

      function As_Object_Access is new Ada.Unchecked_Conversion
        (Abstract_Object_Access, Object_Access);
      function As_Abstract_Object_Access is new Ada.Unchecked_Conversion
        (Object_Access, Abstract_Object_Access);

      function As_Dispatcher_Type is new Ada.Unchecked_Conversion
        (Abstract_Dispatcher_Type, Dispatcher_Type);
      function As_Abstract_Dispatcher_Type is new Ada.Unchecked_Conversion
        (Dispatcher_Type, Abstract_Dispatcher_Type);

      procedure Free is new Ada.Unchecked_Deallocation
        (Object_Type, Object_Access);

      procedure Release_AOA (Self : Abstract_Object_Access);
      procedure Release_AOA (Self : Abstract_Object_Access) is
         OA : Object_Access := As_Object_Access (Self);
      begin
         Free (OA);
      end Release_AOA;

      -------------------
      -- Create_Object --
      -------------------
      procedure Create_Object
        (Table : in out Dispatch_Table; Path : D_Bus.Types.Basic.Object_Path)
      is
      begin
         if Table.Objects.Contains (Path) then
            raise Duplicate_Object with String (Path);
         end if;

         declare
            O_R   : Object_Record;
            O_RSP : Object_Record_SP.Ref;
         begin
            O_R.Alias   := As_Abstract_Object_Access (new Object_Type);
            O_R.Tag     := Tag_Check'Tag;
            O_R.Release := As_Release_Function (Release_AOA'Address);
            O_RSP.Set (O_R);

            Table.Objects.Insert (Path, O_RSP);
         end;
      end Create_Object;

      procedure Dispatch_Transformer
        (Table  : in out Dispatch_Table; Dispatcher : Abstract_Dispatcher_Type;
         Object :        Object_Record; M : D_Bus.Messages.Message);

      procedure Dispatch_Transformer
        (Table  : in out Dispatch_Table; Dispatcher : Abstract_Dispatcher_Type;
         Object :        Object_Record; M : D_Bus.Messages.Message)
      is
         use type Ada.Tags.Tag;
      begin
         --  Prevent calling dispatchers with invalid object types
         --  Note IMPORTANT SAFETY FEATURE
         if Object.Tag /= Tag_Check'Tag then
            Log
              (Warning,
               "Type Error: " & Ada.Tags.Expanded_Name (Object.Tag) & " /= " &
               Ada.Tags.Expanded_Name (Tag_Check'Tag));
            Try_Send_Error
              (Table, M, D_Bus.Errors.Not_Supported,
               "Object incompatible with interface.");
            return;
         end if;

         As_Dispatcher_Type (Dispatcher)
           (Table   => Table, Object => As_Object_Access (Object.Alias).all,
            Message => M);
      end Dispatch_Transformer;

      --------------------
      -- Add_Dispatcher --
      --------------------
      procedure Add_Dispatcher
        (Table       : in out Dispatch_Table;
         M_Interface :        D_Bus.Types.Extra.Interface_Name;
         Dispatcher  :        Dispatcher_Type)
      is
         DR : Dispatcher_Record;
      begin
         if Table.Dispatchers.Contains (M_Interface) then
            raise Duplicate_Dispatcher with String (M_Interface);
         end if;

         DR.Actual  := As_Abstract_Dispatcher_Type (Dispatcher);
         DR.Wrapper := As_Dispatch_Transformer (Dispatch_Transformer'Address);
         Table.Dispatchers.Insert (M_Interface, DR);
      end Add_Dispatcher;
   end Generic_Dispatching;
end D_Bus.Dispatching;
