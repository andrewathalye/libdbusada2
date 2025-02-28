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
   function Is_Valid (Table : U_Dispatch_Table) return Boolean is
     (Table.Valid);

   function Create
     (Connection : in out D_Bus.Connection.Connection) return Dispatch_Table
   is
   begin
      return Result : aliased Dispatch_Table do
         D_Bus.Connection.Move (Connection, Result.Connection);
         Result.Self  := Result'Unchecked_Access;
         Result.Valid := True;
      end return;
   end Create;

   procedure Destroy
     (Table      : out U_Dispatch_Table;
      Connection : out D_Bus.Connection.Connection)
   is
   begin
      D_Bus.Connection.Move (Table.Connection, Connection);
      Table.Valid := False;
   end Destroy;

   ------------------------
   -- Message Management --
   ------------------------
   procedure Update
     (Table : in out Dispatch_Table; Timeout : GNAT.Sockets.Selector_Duration);
   procedure Update
     (Table : in out Dispatch_Table; Timeout : GNAT.Sockets.Selector_Duration)
   is
      M : D_Bus.Messages.Message;
   begin
      if D_Bus.Connection.Can_Read (Table.Connection, Timeout) then
         while D_Bus.Connection.Can_Read (Table.Connection, 0.0) loop
            D_Bus.Connection.Receive (Table.Connection, M);
            Table.Messages.Append (M);
         end loop;
      end if;
   end Update;

   function Send
     (Table : in out Dispatch_Table; Message : D_Bus.Messages.Message)
      return D_Bus.Messages.Message
   is
      Sent    : D_Bus.Messages.Message := Message;
      Timeout : GNAT.Sockets.Selector_Duration;
      Cursor  : Message_Lists.Cursor   := Message_Lists.No_Element;

      function Search_Table return Boolean;
      function Search_Table return Boolean is
      begin
         for C in Table.Messages.Iterate loop
            if D_Bus.Messages.Is_Reply (Message, Message_Lists.Element (C))
            then
               Cursor := C;
               return True;
            end if;
         end loop;

         return False;
      end Search_Table;
   begin
      D_Bus.Connection.Send (Table.Connection, Sent);

      if not Search_Table then
         if D_Bus.Messages.Flags (Message).Allow_Interactive_Authentication
         then
            Timeout := 120.0;
         else
            Timeout := 2.0;
         end if;
         Update (Table, Timeout);

         if not Search_Table then
            raise No_Reply;
         end if;
      end if;

      return
        M : constant D_Bus.Messages.Message := Message_Lists.Element (Cursor)
      do
         Table.Messages.Delete (Cursor);
      end return;

   end Send;

   procedure Send
     (Table : in out Dispatch_Table; Message : D_Bus.Messages.Message)
   is
      Sent : D_Bus.Messages.Message := Message;
   begin
      D_Bus.Connection.Send (Table.Connection, Sent);
   end Send;

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
