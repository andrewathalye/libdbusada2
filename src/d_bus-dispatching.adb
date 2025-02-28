pragma Ada_2012;

with Ada.Unchecked_Conversion;
with Ada.Unchecked_Deallocation;
with System;

with GNAT.Sockets;

with D_Bus.Logging; use D_Bus.Logging;
with System.Address_Image;

package body D_Bus.Dispatching is

   -------------------------------
   -- Dispatch Table Management --
   -------------------------------
   function Create
     (Connection : in out D_Bus.Connection.Connected_Connection)
      return Dispatch_Table
   is
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

   procedure Dispatch
     (Table : in out Dispatch_Table; Timeout : GNAT.Sockets.Selector_Duration);
   --  Internal variant of Dispatch with Timeout specification

   procedure Dispatch
     (Table : in out Dispatch_Table; Timeout : GNAT.Sockets.Selector_Duration)
   is
      use D_Bus.Messages;

      M       : D_Bus.Messages.Message;
      Handled : Boolean := False;
   begin
      --  Get all available messages
      --  Wait at most `Timeout`
      if D_Bus.Connection.Can_Read (Table.Connection, Timeout) then
         while D_Bus.Connection.Can_Read (Table.Connection, 0.0) loop
            D_Bus.Connection.Receive (Table.Connection, M);
            Table.Receive.Append (M);
         end loop;
      end if;

      --  Iterate over list without using cursors
      --  The size of the list is likely to change
      --  while we iterate.
      while not Table.Receive.Is_Empty loop
         --  It is permissible to recursively call into Dispatch.
         --  This guarantees that the next call will see a fresh message
         M := Table.Receive.First_Element;
         Table.Receive.Delete_First;

         case M_Type (M) is
            when Method_Call =>
               if Table.Objects.Contains (Path (M))
                 and then Table.Dispatchers.Contains (M_Interface (M))
               then
                  declare
                     O_R : constant Object_Record_SP.Ref :=
                       Table.Objects (Path (M));
                     D_R : constant Dispatcher_Record    :=
                       Table.Dispatchers (M_Interface (M));
                  begin
                     D_R.Wrapper (Table, D_R.Actual, O_R.Get, M);
                  end;
               else
                  Log
                    (Error,
                     "TODO we don't yet return an error message for this");
               end if;
            when Method_Return | Error =>
               null;
            when Signal =>
               Log (Error, "TODO We do not yet handle signals!");
               Handled := True;
            when Invalid =>
               raise Program_Error;
         end case;

         --  Return the message to the front if we could not process.
         if not Handled then
            Table.Receive.Prepend (M);
         end if;
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
      D_Bus.Connection.Send (Table.Connection, Sent);

      --  Update table
      --  Interactive authentication messages must be given longer
      --  TODO is this sufficient? what if another random message arrives?
      if D_Bus.Messages.Flags (Message).Allow_Interactive_Authentication then
         Dispatch (Table, 120.0);
      else
         Dispatch (Table, 2.0);
      end if;

      --  Try to find the reply
      for C in Table.Receive.Iterate loop
         if D_Bus.Messages.Is_Reply (Message, Message_Lists.Element (C)) then
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
         Table.Receive.Delete (Cursor);
      end return;
   end Send;

   procedure Send
     (Table : in out Dispatch_Table; Message : D_Bus.Messages.Message)
   is
      Sent : D_Bus.Messages.Message := Message;
   begin
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
            O_R.Release := As_Release_Function (Release_AOA'Address);
            O_RSP.Set (O_R);

            Table.Objects.Insert (Path, O_RSP);
         end;
      end Create_Object;

      procedure Dispatch_Transformer
        (Table  : access Dispatch_Table; Dispatcher : Abstract_Dispatcher_Type;
         Object : Object_Record; M : D_Bus.Messages.Message);

      procedure Dispatch_Transformer
        (Table  : access Dispatch_Table; Dispatcher : Abstract_Dispatcher_Type;
         Object : Object_Record; M : D_Bus.Messages.Message)
      is
         use type Ada.Tags.Tag;
      begin
         Log
           (Info,
            "Redirecting to real dispatcher at " &
            System.Address_Image (Dispatcher.all'Address));

         --  TODO test if this works as expected
         if Object.Tag /= Tag_Check'Tag then
            raise Ada.Tags.Tag_Error
              with "Dispatcher cannot handle " &
              Ada.Tags.Expanded_Name (Object.Tag);
         end if;

         As_Dispatcher_Type (Dispatcher)
           (Table => Table.all, Object => As_Object_Access (Object.Alias).all,
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
