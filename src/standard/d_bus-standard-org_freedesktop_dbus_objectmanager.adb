pragma Ada_2012;

with D_Bus.Errors;
with D_Bus.Messages;

package body D_Bus.Standard.org_freedesktop_DBus_ObjectManager is

   -----------------------
   -- Object_Management --
   -----------------------
   package body Object_Management is
      procedure M_Create is new D_Bus.Dispatching.Create_Object
        (Managed_Object);

      package M_Query is new D_Bus.Dispatching.Generic_Object_Query
        (Managed_Object);

      -------------------
      -- Create_Object --
      -------------------
      function Create_Object
        (Table  : in out D_Bus.Dispatching.Dispatch_Table;
         Object : in out Iface'Class; Path : D_Bus.Types.Basic.Object_Path)
         return Managed_Object_Holder
      is
      begin
         if Object.Data.Managed_Objects.Contains (+Path) then
            raise Object_Already_Managed;
         end if;

         Object.Data.Managed_Objects.Append (+Path);
         M_Create (Table, Path);
         return
           (X => M_Query.Query_Object (Table, Path).X.all'Unchecked_Access);
      end Create_Object;

      --------------------
      -- Destroy_Object --
      --------------------
      procedure Destroy_Object
        (Table  : in out D_Bus.Dispatching.Dispatch_Table;
         Object : in out Iface'Class; Path : D_Bus.Types.Basic.Object_Path)
      is
      begin
         if not Object.Data.Managed_Objects.Contains (+Path) then
            raise Object_Not_Managed;
         end if;

         Object.Data.Managed_Objects.Delete
           (Object.Data.Managed_Objects.Find_Index (+Path));
         D_Bus.Dispatching.Destroy_Object (Table, Path);
      end Destroy_Object;

   end Object_Management;

   --------------
   -- Register --
   --------------
   package Dispatching is new D_Bus.Dispatching.Generic_Dispatcher_Management
     (Iface'Class);

   ----------------------------------------
   -- Internals for querying object data --
   ----------------------------------------
   package Classwide_Query is new D_Bus.Dispatching.Generic_Object_Query
     (D_Bus.Standard.org_freedesktop_DBus_Properties.Iface'Class);

   subtype Dict_OARSARSVRR is
     D_Bus.Types.Containers.Dict
       (D_Bus.Types.Object_Path_CC, D_Bus.Types.Intern ("a{sa{sv}}"));
   --  subtype Dict_SARSVR is
   --    D_Bus.Types.Containers.Dict
   --      (D_Bus.Types.String_CC, D_Bus.Types.Intern ("{sv}"));
   --  subtype Dict_SV is
   --    D_Bus.Types.Containers.Dict
   --      (D_Bus.Types.String_CC, D_Bus.Types.Intern ("v"));

   procedure Dispatcher
     (Table  : in out D_Bus.Dispatching.Dispatch_Table;
      Object : in out Iface'Class; Message : D_Bus.Messages.Message);

   procedure Dispatcher
     (Table  : in out D_Bus.Dispatching.Dispatch_Table;
      Object : in out Iface'Class; Message : D_Bus.Messages.Message)
   is
      Member :
        D_Bus.Types.Extra.Member_Name renames D_Bus.Messages.Member (Message);
      Sender :
        D_Bus.Types.Extra.Bus_Name renames D_Bus.Messages.Sender (Message);
      Reply  : D_Bus.Messages.Message;
   begin
      if Member = "GetManagedObjects" then
         if D_Bus.Messages.Signature (Message)'Length /= 0 then
            Reply :=
              D_Bus.Messages.Compose_Error
                (Error => D_Bus.Errors.Invalid_Signature, Reply_To => Message,
                 Destination => Sender);

            goto Send;
         end if;

         Reply :=
           D_Bus.Messages.Compose_Return
             (Reply_To => Message, Destination => Sender);

         --  Reply Signature: a{oa{sa{sv}}}
         --  {Object_Path => {Interface => {Name => Value}}}
         declare
            Objects : Dict_OARSARSVRR;
            Arguments : D_Bus.Types.Argument_List;
         begin
            for Path of Object.Data.Managed_Objects loop
               --  Not correct btw TODO
               --  We actually need to add interfaces with properties
               --  and interfaces WITHOUT properties.
               --
               --  Probably will require something like traversing Ada.Tags
               Objects.Insert
                 (Path,
                  Classwide_Query.Query_Object (Table, +Path).Data.Properties);
            end loop;

            Arguments.Append (Objects);
            D_Bus.Messages.Add_Arguments (Reply, Arguments);
         end;
      else
         Reply :=
           D_Bus.Messages.Compose_Error
             (Error       => D_Bus.Errors.Unknown_Method, Reply_To => Message,
              Destination => Sender);
      end if;

      <<Send>>
      D_Bus.Dispatching.Send (Table, Reply);
   end Dispatcher;

   procedure Register is
   begin
      Dispatching.Register_Dispatcher
        ("org.freedesktop.DBus.ObjectManager", Dispatcher'Access);
   end Register;

end D_Bus.Standard.org_freedesktop_DBus_ObjectManager;
