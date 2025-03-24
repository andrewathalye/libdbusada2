pragma Ada_2012;

with D_Bus.Dispatching;
with D_Bus.Messages;

package D_Bus.Standard.org_freedesktop_DBus_Peer is
   type Iface is interface;

   procedure Ping
     (Table : in out D_Bus.Dispatching.Dispatch_Table;
      Iface : in out Iface'Class;
      Message : D_Bus.Messages.Message);

   procedure GetMachineID
     (Table : in out D_Bus.Dispatching.Dispatch_Table;
      Iface : in out Iface'Class;
      Message : D_Bus.Messages.Message);

   procedure Register;
   --  Register globally as an implementation for this interface.
end D_Bus.Standard.org_freedesktop_DBus_Peer;
