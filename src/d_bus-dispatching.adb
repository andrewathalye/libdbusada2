pragma Ada_2012;

with Ada.Unchecked_Conversion;
with Ada.Unchecked_Deallocation;
with GNAT.Sockets;

with D_Bus.Logging; use D_Bus.Logging;

package body D_Bus.Dispatching is

   ------------
   -- Update --
   ------------
   procedure Update
     (Table   : in out Dispatch_Table; C : aliased D_Bus.Connection.Connection;
      Timeout :        GNAT.Sockets.Selector_Duration);
   procedure Update
     (Table   : in out Dispatch_Table; C : aliased D_Bus.Connection.Connection;
      Timeout :        GNAT.Sockets.Selector_Duration)
   is
      M : D_Bus.Messages.Message;
   begin
      if D_Bus.Connection.Check (C, Timeout) then
         while D_Bus.Connection.Check (C, 0.0) loop
            D_Bus.Connection.Receive (C, M);
            Table.Messages.Append (M);
         end loop;
      end if;
   end Update;

   --------------
   -- Dispatch --
   --------------
   procedure Dispatch
     (Table : in out Dispatch_Table; C : aliased D_Bus.Connection.Connection)
   is
      use D_Bus.Messages;
      use Message_Lists;

      Delete_Cursor : Message_Lists.Cursor;
   begin
      Update (Table, C, GNAT.Sockets.Forever);

      for Cursor in Table.Messages.Iterate loop
         declare
            M : D_Bus.Messages.Message renames Message_Lists.Element (Cursor);
         begin
            case M_Type (M) is
               when Method_Call =>
                  declare
                     P     : D_Bus.Types.Basic.Object_Path renames Path (M);
                     I     :
                       D_Bus.Types.Extra.Interface_Name renames
                       M_Interface (M);
                     Reply : Message;
                  begin
                     if Table.Objects.Contains (P) then
                        if Table.Dispatchers.Contains (I) then
                           Delete_Cursor := Cursor;

                           declare
                              Actual :
                                Abstract_Dispatcher renames
                                Table.Dispatchers.Element (I).Actual;

                              Wrapper_Address :
                                System.Address renames
                                Table.Dispatchers.Element (I).Wrapper;

                              function Wrapper
                                (Connection : aliased D_Bus.Connection
                                   .Connection;
                                 Dispatcher :         Abstract_Dispatcher;
                                 Object     :         Abstract_Object_Access;
                                 M          :         D_Bus.Messages.Message)
                                 return D_Bus.Messages.Message with
                                Import  => True, Convention => Ada,
                                Address => Wrapper_Address;

                              Object :
                                Abstract_Object_Access renames
                                Table.Objects.Element (P).Get.Alias;
                           begin
                              Reply := Wrapper (C, Actual, Object, M);
                           end;

                           D_Bus.Connection.Send (C, Reply);
                           goto Done;
                        end if;
                     end if;
                  end;
                  Log (Warning, "TODO no obj or dispatcher found");
                  <<Done>>
               when Signal =>
                  Log (Error, "TODO signals unimplemented by dispatcher");
               when Method_Return | Error =>
                  null;
               when Invalid =>
                  raise Program_Error; --  Note: Checked by Read_RMH
            end case;
         end;
      end loop;

      if Delete_Cursor /= Message_Lists.No_Element then
         Table.Messages.Delete (Delete_Cursor);
      end if;
   end Dispatch;

   function Get_Reply
     (Table : in out Dispatch_Table; C : aliased D_Bus.Connection.Connection;
      Original :        D_Bus.Messages.Message) return D_Bus.Messages.Message
   is
      use D_Bus.Messages;
      use type Message_Lists.Cursor;

      Computed_Duration : GNAT.Sockets.Selector_Duration;
      Cursor            : Message_Lists.Cursor := Message_Lists.No_Element;

      procedure Table_Check;
      procedure Table_Check is
      begin
         for C in Table.Messages.Iterate loop
            case M_Type (Message_Lists.Element (C)) is
               when Method_Return | Error =>
                  if Is_Reply (Original, Message_Lists.Element (C)) then
                     Cursor := C;
                     exit;
                  end if;
               when Method_Call | Signal =>
                  null;
               when Invalid =>
                  raise Program_Error;
            end case;
         end loop;
      end Table_Check;
   begin
      if Flags (Original).No_Reply_Expected then
         raise No_Reply;
      end if;

      --  Check for a reply
      Table_Check;

      --  Get fresh data if there is still nothing
      if Cursor = Message_Lists.No_Element then
         --  Maximum time we are willing to wait for a reply
         --  TODO is this a good enough solution?
         if Flags (Original).Allow_Interactive_Authentication then
            Computed_Duration := 120.0;
         else
            Computed_Duration := 2.0;
         end if;

         Update (Table, C, Computed_Duration);
         Table_Check;

         --  Check once again and error out if nothing
         if Cursor = Message_Lists.No_Element then
            raise No_Reply;
         end if;
      end if;

      declare
         M : constant Message := Message_Lists.Element (Cursor);
      begin
         Table.Messages.Delete (Cursor);
         return M;
      end;
   end Get_Reply;

   --------------------
   -- Destroy_Object --
   --------------------
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

   -----------------------
   -- Remove_Dispatcher --
   -----------------------
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

   -------------
   -- Release --
   -------------
   procedure Release (Self : in out Object_Record) is
      procedure Release_AOA (Self : in out Abstract_Object_Access) with
        Import => True, Convention => Ada, Address => Self.Release;
   begin
      Release_AOA (Self.Alias);
   end Release;

   -------------------------
   -- Generic_Dispatching --
   -------------------------
   package body Generic_Dispatching is
      type Object_Access is access all Object_Type;
      type Tag_Check is tagged null record;

      function Convert is new Ada.Unchecked_Conversion
        (Abstract_Object_Access, Object_Access);
      function Convert is new Ada.Unchecked_Conversion
        (Object_Access, Abstract_Object_Access);
      function Convert is new Ada.Unchecked_Conversion
        (Abstract_Dispatcher, Dispatcher_Type);
      function Convert is new Ada.Unchecked_Conversion
        (Dispatcher_Type, Abstract_Dispatcher);

      procedure Free is new Ada.Unchecked_Deallocation
        (Object_Type, Object_Access);

      procedure Release_AOA (Self : Abstract_Object_Access);
      procedure Release_AOA (Self : Abstract_Object_Access) is
         OA : Object_Access := Convert (Self);
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
            O_R.Alias   := Convert (new Object_Type);
            O_R.Release := Release_AOA'Address;
            O_RSP.Set (O_R);

            Table.Objects.Insert (Path, O_RSP);
         end;
      end Create_Object;

      ------------------
      -- Query_Object --
      ------------------
      function Query_Object
        (Table : in out Dispatch_Table; Path : D_Bus.Types.Basic.Object_Path)
         return Object_Reference
      is
      begin
         if not Table.Objects.Contains (Path) then
            raise No_Object with String (Path);
         end if;

         declare
            use type Ada.Tags.Tag;
            O_R :
              Object_Record renames
              Table.Objects.Element (Path).Unchecked_Get.all;
         begin
            if O_R.Tag /= Tag_Check'Tag then
               raise Constraint_Error with "Wrong tag";
            end if;

            return Object_Reference'(X => Convert (O_R.Alias));
         end;
      end Query_Object;

      function Dispatch_Transformer
        (Connection : aliased D_Bus.Connection.Connection;
         Dispatcher : Abstract_Dispatcher; Object : Abstract_Object_Access;
         M          : D_Bus.Messages.Message) return D_Bus.Messages.Message;
      function Dispatch_Transformer
        (Connection : aliased D_Bus.Connection.Connection;
         Dispatcher : Abstract_Dispatcher; Object : Abstract_Object_Access;
         M          : D_Bus.Messages.Message) return D_Bus.Messages.Message
      is
      begin
         return Convert (Dispatcher) (Connection, Convert (Object).all, M);
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

         DR.Actual  := Convert (Dispatcher);
         DR.Wrapper := Dispatch_Transformer'Address;
         Table.Dispatchers.Insert (M_Interface, DR);
      end Add_Dispatcher;
   end Generic_Dispatching;
end D_Bus.Dispatching;
