with GNATCOLL.OS.Process;
package D_Bus.Platform is
   function Get_Machine_ID return String;
   pragma Pure_Function (Get_Machine_ID);
   --  Return the machine ID of the current host.
   --  This is an abstract, implementation defined concept
   --  that is constant for any execution on the same hardware
   --  and software.
   --
   --  This implementation aims to produce the same value as
   --  the reference implementation in all cases.

   function Is_Running
     (Handle : GNATCOLL.OS.Process.Process_Handle) return Boolean;
   --  Return true if there is a process running on the current machine
   --  with the given handle (semantics differ by OS).
end D_Bus.Platform;
