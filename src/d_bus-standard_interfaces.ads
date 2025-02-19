package D_Bus.Standard_Interfaces is
   --  Standard interfaces documented in the documentation
   
   package org_freedesktop_DBus_Peer is
      function GetMachineId return String;
      pragma Pure_Function (GetMachineId);
   end org_freedesktop_DBus_Peer;
end D_Bus.Standard_Interfaces;
