pragma Ada_2012;

with D_Bus.Platform_Dependent;

package body D_Bus.Standard_Interfaces is

   -------------------------------
   -- org_freedesktop_DBus_Peer --
   -------------------------------
   package body org_freedesktop_DBus_Peer is
      Machine_ID : constant String := D_Bus.Platform_Dependent.Get_Machine_ID;

      ------------------
      -- GetMachineId --
      ------------------
      function GetMachineId return String is (Machine_ID);
   end org_freedesktop_DBus_Peer;

end D_Bus.Standard_Interfaces;
