pragma Ada_2012;

with GNAT.OS_Lib;
with GNAT.Sockets;
with GNATCOLL.OS.Process;

with D_Bus.Types;

package D_Bus.Platform is
   function Get_Machine_ID return D_Bus.Types.UUID;
   pragma Pure_Function (Get_Machine_ID);
   --  Return the machine ID of the current host.
   --  This is an abstract, implementation defined concept
   --  that is constant for any execution on the same hardware
   --  and software.
   --
   --  This implementation aims to produce the same value as
   --  the reference implementation in all cases.

   function Get_User_ID return String;
   pragma Pure_Function (Get_User_ID);
   --  Return the ID of the user under whom the library
   --  code is currently executing. The result is an
   --  opaque value defined by the current platform.

   function Is_Running
     (Handle : GNATCOLL.OS.Process.Process_Handle) return Boolean;
   --  Return true if there is a process running on the current machine
   --  with the given handle (semantics differ by OS).

   procedure Read_FD
     (Socket : GNAT.Sockets.Socket_Type; FD : out GNAT.OS_Lib.File_Descriptor);
   procedure Write_FD
     (Socket : GNAT.Sockets.Socket_Type; FD : GNAT.OS_Lib.File_Descriptor);
   --  Read / write a file descriptor over a socket.
   --  This may return OS_Error if it is not supported.

   function FD_Transfer_Support return Boolean;
   pragma Pure_Function (FD_Transfer_Support);
end D_Bus.Platform;
