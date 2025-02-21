package D_Bus.Platform is
   function Get_Machine_ID return String
   is
   begin
      --  TODO we should implement somehow. Either iterate interfaces for
      --  MAC address or somehow get the Mac’s serial number.
      return raise Program_Error with "Unimplemented for macOS";
   end Get_Machine_ID;
end D_Bus.Platform;
