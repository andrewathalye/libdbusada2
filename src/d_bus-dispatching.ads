pragma Ada_2012;

with D_Bus.Connection;
with D_Bus.Messages;
with D_Bus.Types.Basic;
with D_Bus.Types.Extra;

private with Ada.Containers.Doubly_Linked_Lists;
private with Ada.Containers.Indefinite_Hashed_Maps;
private with Ada.Strings.Hash;
private with Ada.Tags;

private with GNATCOLL.Refcount;

package D_Bus.Dispatching is
   pragma Assertion_Policy (Pre => Check);

   -------------------------------
   -- Dispatch Table Management --
   -------------------------------
   type Dispatch_Table (<>) is limited private;
   --  Represents a data table containing
   --  associations between objects and paths
   --  and interfaces and handler functions.
   --
   --  Note: A dispatcher must not do any of the following:
   --  1) Destroy its own dispatching table TODO
   --  2) Call function `Send`
   --     a) If the destination and sender are the same.
   --     b) If it is known that the destination will call
   --        a method or wait for a signal from the sender.
   --
   --  Violating constraint 1 is exceptional behaviour.
   --  Violating constraint 2 will violate the time constraints below.

   --  Note: Trying to create a subtype for valid dispatch tables
   --  caused a compiler crash, so instead the type is implemented
   --  such that all values are 'valid', but using it with an
   --  invalid connection will be handled on attempting to send data.

   function Is_Connected (Table : Dispatch_Table) return Boolean;
   --  Return whether `Table` currently has a connection.

   procedure Create
     (Connection : in out D_Bus.Connection.Connection;
      Table      :    out Dispatch_Table) with
     Pre =>
      Connection in D_Bus.Connection.Connected_Connection and
      not Is_Connected (Table);
   --  Assign `Connection` to `Table`. `Table` must not have an
   --  associated connection.

   function Create
     (Connection : in out D_Bus.Connection.Connection)
      return Dispatch_Table with
     Pre => Connection in D_Bus.Connection.Connected_Connection;
   --  Create a new dispatch table, consuming `Connection` and assigning it
   --  to the table. `Connection` will no longer be usable after this process.

   function Create return Dispatch_Table;
   --  Create a new dispatch table using the default session bus.

   procedure Destroy
     (Table      : in out Dispatch_Table;
      Connection :    out D_Bus.Connection.Connection);
   --  Return control of `Connection` to the caller.
   --  This also clears all data in `Table`

   ------------------------
   -- Message Management --
   ------------------------
   No_Reply : exception;
   use type D_Bus.Messages.Message_Type;
   function Send
     (Table : in out Dispatch_Table; Message : D_Bus.Messages.Message)
      return D_Bus.Messages.Message with
     Pre =>
      D_Bus.Messages.M_Type (Message) = D_Bus.Messages.Method_Call and
      not D_Bus.Messages.Flags (Message).No_Reply_Expected;
   --  Sends `Message` via `Table` and return its reply.
   --
   --  Raise `No_Reply` if no reply was received after an
   --  implementation-defined amount of time.

   procedure Send
     (Table : in out Dispatch_Table; Message : D_Bus.Messages.Message) with
     Pre =>
      D_Bus.Messages.M_Type (Message) /= D_Bus.Messages.Method_Call or
      D_Bus.Messages.Flags (Message).No_Reply_Expected;
   --  Sends `Message` via `Table`
   --  It is an error to call this with a message that expects a reply.

   No_Signal : exception;
   subtype Await_Duration is Duration range -1.0 .. Duration'Last;
   Forever : constant Await_Duration;

   function Await
     (Table : in out Dispatch_Table; Path : D_Bus.Types.Basic.Object_Path;
      M_Interface :        D_Bus.Types.Extra.Interface_Name;
      Member      :        D_Bus.Types.Extra.Member_Name;
      Timeout     : Await_Duration := Forever) return D_Bus.Messages.Message;
   --  Return a signal `Member` on interface `M_Interface`
   --  from the object with path `Path`.
   --
   --  Wait for up to Timeout + 1 seconds (or Forever)
   --  Raise `No_Signal` on failure.

   procedure Dispatch (Table : in out Dispatch_Table);
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

   -------------------------
   -- Generic Dispatching --
   -------------------------
   generic
      type Object_Type is limited private;
   package Generic_Dispatching is
      --  The power of this package is in its ability to support _generic_
      --  dispatching. Instantiate this package with any constrained
      --  subtype and you will be able to add dispatchers and objects of
      --  that type.

      procedure Create_Object
        (Table : in out Dispatch_Table; Path : D_Bus.Types.Basic.Object_Path);
      --  Create an opaque object associated with `Path`.
      --  An access to it will be returned on every call to a `Dispatcher`
      --  function.
      --
      --  Raise `Duplicate_Object` if the object already exists.

      type Dispatcher_Type is
        access procedure
          (Table   : in out Dispatch_Table; Object : in out Object_Type;
           Message :        D_Bus.Messages.Message);
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
   -----------------------------
   -- Private Implementations --
   -----------------------------
   type Abstract_Object_Type is limited null record;
   type Abstract_Object_Access is access Abstract_Object_Type;

   type Release_Function is
     access procedure (Self : in out Abstract_Object_Access);

   type Object_Record is record
      Alias   : Abstract_Object_Access;
      Tag     : Ada.Tags.Tag := Ada.Tags.No_Tag;
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
       (Table   : in out Dispatch_Table; Object : in out Abstract_Object_Type;
        Message :        D_Bus.Messages.Message);

   type Dispatch_Transformer is
     access procedure
       (Table  : in out Dispatch_Table; Dispatcher : Abstract_Dispatcher_Type;
        Object :        Object_Record; M : D_Bus.Messages.Message);

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

   type Dispatch_Table is limited record
      Connection  : aliased D_Bus.Connection.Connection;
      Messages    : Message_Lists.List;
      Replies     : Message_Lists.List;
      Signals     : Message_Lists.List;
      Objects     : Object_Path_Maps.Map;
      Dispatchers : Dispatcher_Interface_Maps.Map;
   end record;

   --------------------------------
   -- Completions of Public Part --
   --------------------------------
   function Is_Connected (Table : Dispatch_Table) return Boolean is
     (D_Bus.Connection.Is_Connected (Table.Connection));

   Forever : constant Await_Duration := Await_Duration'Last;
end D_Bus.Dispatching;
