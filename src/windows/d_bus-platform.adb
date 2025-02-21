pragma Ada_2012;

with Interfaces.C;

package body D_Bus.Platform_Dependent is
   ---------------
   -- Constants --
   ---------------
   HW_PROFILE_GUIDLEN : constant := 39;
   MAX_PROFILE_LEN    : constant := 80;

   -----------
   -- Types --
   -----------
   type HW_PROFILE_INFOA is record
      dwDockInfo      : Interfaces.Unsigned_32;
      szHwProfileGuid : Interfaces.C.char_array (1 .. HW_PROFILE_GUIDLEN);
      szHwProfileName : Interfaces.C.char_array (1 .. MAX_PROFILE_LEN);
   end record;

   ---------------
   -- Functions --
   ---------------
   function GetCurrentHwProfileA
     (lpHwProfileInfo : access HW_PROFILE_INFOA) return Interfaces.C.C_bool
   with
     Import        => True,
     Convention    => C,
     External_Name => "GetCurrentHwProfileA";

   --------------------
   -- Implementation --
   --------------------
   function Get_Machine_ID return String is
      Hardware_Profile : aliased HW_PROFILE_INFOA;
      Success          : Interfaces.C.C_bool;
   begin
      --  Try to fetch the hardware profile
      Success := GetCurrentHwProfileA (Hardware_Profile'Access);

      if not Success then
         raise Program_Error with "Could not get hardware profile info";
      end if;

      --  Crop out { and } at beginning and end
      declare
         Result : constant String :=
           Interfaces.C.To_Ada (Hardware_Profile.szHwProfileGuid);
      begin
         return Result (Result'First + 1 .. Result'Last - 1);
      end;
   end Get_Machine_ID;
end D_Bus.Platform_Dependent;
