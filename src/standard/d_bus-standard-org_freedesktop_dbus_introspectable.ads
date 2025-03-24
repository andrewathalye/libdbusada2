pragma Ada_2012;

with D_Bus.Dispatching;
with D_Bus.Messages;

package D_Bus.Standard.org_freedesktop_DBus_Introspectable is
   type Iface is interface and D_Bus.Standard.Server_Interface;

   function Introspect (Object : Iface) return String is abstract;
   --  To be implemented by any objects which wish to declare
   --  themselves introspectable.
   --
   --  The result must be valid XML, but this is NOT checked.

   procedure Register;
   --  Register as the handler for this interface.
end D_Bus.Standard.org_freedesktop_DBus_Introspectable;
