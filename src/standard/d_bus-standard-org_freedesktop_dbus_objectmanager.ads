pragma Ada_2012;

with D_Bus.Dispatching;
with D_Bus.Standard.org_freedesktop_DBus_Properties;
with D_Bus.Types.Basic;

package D_Bus.Standard.org_freedesktop_DBus_ObjectManager is
   type Iface is interface and D_Bus.Standard.Server_Interface;

   -----------------------
   -- Object Management --
   -----------------------
   Object_Already_Managed : exception;
   Object_Not_Managed     : exception;

   generic
      type Managed_Object is
        new D_Bus.Standard.org_freedesktop_DBus_Properties.Iface with private;
   package Object_Management is
      type Managed_Object_Holder (X : not null access Managed_Object) is
        limited private with
        Implicit_Dereference => X;

      function Create_Object
        (Table  : in out D_Bus.Dispatching.Dispatch_Table;
         Object : in out Iface'Class; Path : D_Bus.Types.Basic.Object_Path)
         return Managed_Object_Holder;
      --  Create a managed object with subpath `Path`
      --
      --  Raise Object_Already_Managed if the object is already managed.

      procedure Destroy_Object
        (Table  : in out D_Bus.Dispatching.Dispatch_Table;
         Object : in out Iface'Class; Path : D_Bus.Types.Basic.Object_Path);
      --  Semantics are identical to `D_Bus.Dispatching` equivalents.
      --  The underlying implementation is not guaranteed to be the same.
      --
      --  Raise Object_Not_Managed if the object is not managed.
   private
      type Managed_Object_Holder (X : not null access Managed_Object) is
      limited null record;
   end Object_Management;

   procedure Register;
   --  Register as the handler for this interface.
end D_Bus.Standard.org_freedesktop_DBus_ObjectManager;
