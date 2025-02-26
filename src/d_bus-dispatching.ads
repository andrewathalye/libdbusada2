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
with System;

package D_Bus.Dispatching is
   -------------------------------
   -- Dispatch Table Management --
   -------------------------------
   type Dispatch_Table is limited private;
   --  Represents a data table containing
   --  associations between objects and paths
   --  and interfaces and handler functions.

   procedure Dispatch
     (Table : in out Dispatch_Table; C : aliased D_Bus.Connection.Connection);
   --  Read all messages waiting in Connection and dispatch them according to
   --  the table rules as defined below. Will block until at least one message
   --  is available.

   No_Reply : exception;
   function Get_Reply
     (Table : in out Dispatch_Table; C : aliased D_Bus.Connection.Connection;
      Original :        D_Bus.Messages.Message) return D_Bus.Messages.Message;
   --  Look for a message replying to `Original` and return it. Will wait
   --  a suitable amount of time for a reply depending on the message.
   --
   --  Raise `No_Reply` if no reply can be found.

   -----------------------
   -- Object Management --
   -----------------------
   Duplicate_Object : exception;
   No_Object        : exception;

   type Abstract_Object is limited private;
   type Abstract_Object_Access is access all Abstract_Object;

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
      type Object_Reference (X : access Object_Type) is
      limited null record with
        Implicit_Dereference => X;

      procedure Create_Object
        (Table : in out Dispatch_Table; Path : D_Bus.Types.Basic.Object_Path);
      --  Create an opaque object associated with `Path`.
      --  An access to it will be returned on every call to a `Dispatcher`
      --  function.
      --
      --  Raise `Duplicate_Object` if the object already exists.

      function Query_Object
        (Table : in out Dispatch_Table; Path : D_Bus.Types.Basic.Object_Path)
         return Object_Reference;
      --  Return a reference to the object with path `Path`
      --  This is the same object that would be passed to a dispatcher.
      --
      --  Raise `No_Object` if no object with that path exists

      type Dispatcher_Type is
        access function
          (C : aliased D_Bus.Connection.Connection; O : in out Object_Type;
           M :         D_Bus.Messages.Message) return D_Bus.Messages.Message;
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
   type Abstract_Object is limited null record;

   type Release_Signature is
     access procedure (Self : in out Abstract_Object_Access);
   type Object_Record is record
      Alias   : Abstract_Object_Access;
      Tag     : Ada.Tags.Tag;
      Release : System.Address;
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

   type Abstract_Dispatcher is access function return D_Bus.Messages.Message;
   type Dispatch_Transformer is
     access function
       (Connection : aliased D_Bus.Connection.Connection;
        Dispatcher : Abstract_Dispatcher; Object : Abstract_Object_Access;
        M          : D_Bus.Messages.Message) return D_Bus.Messages.Message;

   type Dispatcher_Record is record
      Actual  : Abstract_Dispatcher;
      Wrapper : System.Address;
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
      Messages    : Message_Lists.List;
      Objects     : Object_Path_Maps.Map;
      Dispatchers : Dispatcher_Interface_Maps.Map;
   end record;
end D_Bus.Dispatching;
