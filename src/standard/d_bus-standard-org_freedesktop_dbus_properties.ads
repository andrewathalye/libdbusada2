pragma Ada_2012;

with D_Bus.Dispatching;
with D_Bus.Messages;
with D_Bus.Types.Containers;

package D_Bus.Standard.org_freedesktop_DBus_Properties is
   type Iface is interface and D_Bus.Standard.Server_Interface;

   function Get
     (Object         : in out Iface'Class;
      Interface_Name :        D_Bus.Types.Extra.Interface_Name;
      Property_Name  :        D_Bus.Types.Extra.Member_Name)
      return D_Bus.Types.Containers.Variant;

   procedure Set
     (Object         : in out Iface'Class;
      Interface_Name :        D_Bus.Types.Extra.Interface_Name;
      Property_Name  :        D_Bus.Types.Extra.Member_Name;
      Value          :        D_Bus.Types.Containers.Variant);

   function GetAll
     (Object         : in out Iface'Class;
      Interface_Name :        D_Bus.Types.Extra.Interface_Name)
      return D_Bus.Standard.Property_Dict;

   procedure Set_Default_Properties (Object : in out Iface) is null;
   --  Must be overridden if an object is required to have default properties.

   procedure Register;
   --  Register globally as an implementation for this interface.
end D_Bus.Standard.org_freedesktop_DBus_Properties;
