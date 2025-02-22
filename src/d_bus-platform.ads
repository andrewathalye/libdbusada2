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
   --  This may raise an exception if it is not supported.

   function FD_Transfer_Support (S : GNAT.Sockets.Socket_Type) return Boolean;
   --  Returns whether socket `S` supports transferring file descriptors
   --  on the current platform.

   Credentials_Error : exception;
   function Read_Credentials (S : GNAT.Sockets.Socket_Type) return String;
   --  Return the ID of the user who owns socket `S`.
   --
   --  Raise `Credentials_Error` on failure.

   procedure Write_Credentials (S : GNAT.Sockets.Socket_Type);
   --  Send credentials over `S` to be verified by the recipient.
   --  Additionally sends a null byte.
   --
   --  Raise `Credentials_Error` on failure.
   --  On failure, does not write a null byte.
end D_Bus.Platform;
