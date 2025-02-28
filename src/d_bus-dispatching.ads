pragma Ada_2012;

with Ada.Containers.Doubly_Linked_Lists;
with Ada.Strings;
with Ada.Strings.Hash;
with Ada.Tags;
with D_Bus.Connection;
with D_Bus.Messages;
with D_Bus.Types.Basic;
with D_Bus.Types.Extra;

private with Ada.Containers.Indefinite_Hashed_Maps;
private with GNATCOLL.Refcount;

package D_Bus.Dispatching is
   pragma Assertion_Policy (Pre => Check);
   pragma Assertion_Policy (Dynamic_Predicate => Check);

   -------------------------------
   -- Dispatch Table Management --
   -------------------------------
   type U_Dispatch_Table (<>) is limited private;
   function Is_Valid (Table : U_Dispatch_Table) return Boolean;

   subtype Dispatch_Table is U_Dispatch_Table with
       Dynamic_Predicate => Is_Valid (Dispatch_Table);
   --  Represents a data table containing
   --  associations between objects and paths
   --  and interfaces and handler functions.

   function Create
     (Connection : in out D_Bus.Connection.Connection)
      return Dispatch_Table with
     Pre => Connection in D_Bus.Connection.Connected_Connection;
   --  Create a new dispatch table, consuming `Connection` and assigning it
   --  to the table. `Connection` will no longer be usable after this process.
   --
   --  `Connection` must be connected prior to creating a table.
   --  TODO test and reconsider

   procedure Destroy
     (Table      : out U_Dispatch_Table;
      Connection : out D_Bus.Connection.Connection) with
     Pre => Table in Dispatch_Table;
   --  Destroy all data held by `Table`.
   --  `Connection` is returned to the caller
   --  and may be used freely.
   --  idem test and reconsider TODO

   ------------------------
   -- Message Management --
   ------------------------
   No_Reply : exception;
   function Send
     (Table : in out Dispatch_Table; Message : D_Bus.Messages.Message)
      return D_Bus.Messages.Message with
     Pre => not D_Bus.Messages.Flags (Message).No_Reply_Expected;
   --  Sends `Message` via `Table` and return its reply.
   --
   --  Raise `No_Reply` if no reply was received after an
   --  implementation-defined amount of time.

   procedure Send
     (Table : in out Dispatch_Table; Message : D_Bus.Messages.Message) with
     Pre => D_Bus.Messages.Flags (Message).No_Reply_Expected;
   --  Sends `Message` via `Table`
   --  It is an error to call this with a message that expects a reply.

   --  procedure Dispatch (Table : in out Dispatch_Table);
   --  Reads all pending messages and dispatches them according to table rules.
   --  Will block until at least one message has been read.

   -----------------------
   -- Object Management --
   -----------------------
   Duplicate_Object : exception;
   No_Object        : exception;

   procedure Destroy_Object
     (Table : in out Dispatch_Table; Path : D_Bus.Types.Basic.Object_Path);
   --  Destroy the opaque object with path `Path`
   --
   --  Raise `No_Object` if no object with that path exists

   ---------------------------
   -- Dispatcher Management --
   ---------------------------
   Duplicate_Dispatcher : exception;
   No_Dispatcher        : exception;

   procedure Remove_Dispatcher
     (Table       : in out Dispatch_Table;
      M_Interface :        D_Bus.Types.Extra.Interface_Name);
   --  Remove a dispatcher for the specified interface.
   --
   --  Raise `No_Dispatcher` if no dispatcher was registered for `M_Interface`

   generic
      type Object_Type is limited private;
   package Generic_Dispatching is
      procedure Create_Object
        (Table : in out Dispatch_Table; Path : D_Bus.Types.Basic.Object_Path);
      --  Create an opaque object associated with `Path`.
      --  An access to it will be returned on every call to a `Dispatcher`
      --  function.
      --
      --  Raise `Duplicate_Object` if the object already exists.

      type Dispatcher_Type is
        access procedure
          (Table   : Dispatch_Table; Object : in out Object_Type;
           Message : D_Bus.Messages.Message);
      --  Takes a reference to an object and a copy of a message associated
      --   with that object.

      procedure Add_Dispatcher
        (Table       : in out Dispatch_Table;
         M_Interface :        D_Bus.Types.Extra.Interface_Name;
         Dispatcher  :        Dispatcher_Type);
      --  Assign a dispatcher for the specified interface.
      --
      --  Raise `Duplicate_Dispatcher` if the dispatcher already exists.
   end Generic_Dispatching;
private
   type Abstract_Object_Type is limited null record;
   type Abstract_Object_Access is access Abstract_Object_Type;

   type Release_Function is
     access procedure (Self : in out Abstract_Object_Access);

   type Object_Record is record
      Alias   : Abstract_Object_Access;
      Tag     : Ada.Tags.Tag;
      Release : Release_Function;
   end record;

   procedure Release (Self : in out Object_Record);
   package Object_Record_SP is new GNATCOLL.Refcount.Shared_Pointers
     (Object_Record, Release);

   function Hash
     (Item : D_Bus.Types.Basic.Object_Path) return Ada.Containers.Hash_Type is
     (Ada.Strings.Hash (String (Item)));

   use type D_Bus.Types.Basic.Object_Path;
   use type Object_Record_SP.Ref;
   package Object_Path_Maps is new Ada.Containers.Indefinite_Hashed_Maps
     (Key_Type        => D_Bus.Types.Basic.Object_Path,
      Element_Type    => Object_Record_SP.Ref, Hash => Hash,
      Equivalent_Keys => "=");

   type Abstract_Dispatcher_Type is
     access procedure
       (Table   : access Dispatch_Table; Object : in out Abstract_Object_Type;
        Message : D_Bus.Messages.Message);

   type Dispatch_Transformer is
     access procedure
       (Table  : access Dispatch_Table; Dispatcher : Abstract_Dispatcher_Type;
        Object : Object_Record; M : D_Bus.Messages.Message);

   type Dispatcher_Record is record
      Actual  : Abstract_Dispatcher_Type;
      Wrapper : Dispatch_Transformer;
   end record;

   package Dispatcher_Interface_Maps is new Ada.Containers
     .Indefinite_Hashed_Maps
     (Key_Type        => D_Bus.Types.Extra.Interface_Name,
      Element_Type    => Dispatcher_Record, Hash => Ada.Strings.Hash,
      Equivalent_Keys => "=");

   use type D_Bus.Messages.Message;
   package Message_Lists is new Ada.Containers.Doubly_Linked_Lists
     (D_Bus.Messages.Message);

   type U_Dispatch_Table is limited record
      Valid       : Boolean := False;
      Self        : access Dispatch_Table;
      Connection  : aliased D_Bus.Connection.Connection;
      Messages    : Message_Lists.List;
      Objects     : Object_Path_Maps.Map;
      Dispatchers : Dispatcher_Interface_Maps.Map;
   end record;
end D_Bus.Dispatching;
