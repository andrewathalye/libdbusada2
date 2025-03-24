pragma Ada_2012;

with Ada.Containers.Vectors;
with D_Bus.Types.Basic;
with D_Bus.Types.Containers;
with D_Bus.Types.Extra;

package D_Bus.Standard is
   ----------------------
   -- For Implementors --
   ----------------------
   type Server_Interface is interface;
   --  The base type for all serverside D-Bus objects that
   --  integrate with the provided standard library code.

   type Server_Data is private;
   type Server_Data_Holder (X : not null access Server_Data) is
     limited private with
     Implicit_Dereference => X;
   --  Data storage for the standard library code.

   function Data
     (Iface : in out Server_Interface) return Server_Data_Holder is abstract;
   --  Retrieve access to data stored within the object implementing
   --  `Server_Interface`

   ---------------
   -- For Users --
   ---------------
   type Server_Object is new Server_Interface with private;
   --  The recommended base type for serverside D-Bus objects.
   --  This implements `Data` automatically and is easier to use.
private
   subtype Property_Dict is
     D_Bus.Types.Containers.Dict
       (Key_Signature     => D_Bus.Types.String_CC,
        Element_Signature => D_Bus.Types.Intern ("a{sv}"));
   --  [{Interface_Name => [{Property_Name => Property_Value}]}]

   use type D_Bus.Types.Basic.D_Object_Path;
   package Object_Lists is new Ada.Containers.Vectors
     (Natural, D_Bus.Types.Basic.D_Object_Path);

   type Server_Data is record
      Properties      : Property_Dict;
      Managed_Objects : Object_Lists.Vector;
   end record;

   type Server_Data_Holder (X : not null access Server_Data) is
   limited null record;

   type Server_Object is new Server_Interface with record
      I_Data : aliased Server_Data;
   end record;

   overriding function Data
     (Iface : in out Server_Object) return Server_Data_Holder is
     ((X => Iface.I_Data'Unchecked_Access));
end D_Bus.Standard;
