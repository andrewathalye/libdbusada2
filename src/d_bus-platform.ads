pragma Ada_2012;

with Ada.Containers.Vectors;
with Ada.Streams;
with GNAT.OS_Lib;
with GNAT.Sockets;
with GNATCOLL.OS.Process;

with Interfaces;

with D_Bus.Types;

package D_Bus.Platform is
   ----------------
   -- MACHINE ID --
   ----------------
   No_Machine_ID : exception;
   function Get_Machine_ID return D_Bus.Types.UUID;
   pragma Pure_Function (Get_Machine_ID);
   --  Return the machine ID of the current host.
   --  This is an abstract, implementation defined concept
   --  that is constant for any execution on the same
   --  hardware and software within the span of a single
   --  power cycle.
   --
   --  This implementation aims to produce the same value as
   --  the reference implementation in all cases.
   --
   --  On failure:
   --  Raise No_Machine_ID

   -----------
   -- USERS --
   -----------
   function Get_User_ID return String;
   pragma Pure_Function (Get_User_ID);
   --  Return the ID of the user under whom the library
   --  code is currently executing. The result is an
   --  opaque value defined by the current platform.

   ---------------
   -- PROCESSES --
   ---------------
   function Is_Running
     (Handle : GNATCOLL.OS.Process.Process_Handle) return Boolean;
   --  Return true if there is a process running on the current machine
   --  with the given handle.

   ----------------------
   -- FILE DESCRIPTORS --
   ----------------------
   File_Descriptor_Error : exception;
   use type GNAT.OS_Lib.File_Descriptor;
   package FD_Vectors is new
     Ada.Containers.Vectors (Natural, GNAT.OS_Lib.File_Descriptor);
   subtype FD_Vector is FD_Vectors.Vector;

   function File_Descriptor_Passing_Support
     (S : GNAT.Sockets.Socket_Type) return Boolean;
   --  Returns whether socket `S` supports transferring file descriptors
   --  on the current platform.

   procedure Receive_Data_With_FDs
     (Socket : GNAT.Sockets.Socket_Type;
      Item   : out Ada.Streams.Stream_Element_Array;
      Last   : out Ada.Streams.Stream_Element_Offset;
      FDs    : in out FD_Vector);
   --  Read data from a socket and attempt to retrieve FDs transferred on it.
   --  Raise File_Descriptor_Error if file descriptors may have been lost

   procedure Send_Data_With_FDs
     (Socket : GNAT.Sockets.Socket_Type;
      Item   : Ada.Streams.Stream_Element_Array;
      FDs    : FD_Vector);
   --  Write data to a socket and attempt to send FDs over it
   --  Raise File_Descriptor_Error if no file descriptors could be sent

   -----------------
   -- CREDENTIALS --
   -----------------
   Credentials_Error : exception;
   function Read_Credentials (S : GNAT.Sockets.Socket_Type) return String;
   --  Return the ID of the user who owns socket `S`.
   --  This causes an authentication token to be consumed from `S`.
   --
   --  On failure:
   --  Raise `Credentials_Error` if no data was consumed.

   procedure Write_Credentials (S : GNAT.Sockets.Socket_Type);
   --  Send an authentication token over socket `S`
   --  Additionally sends a null byte.
   --
   --  On failure:
   --  Raise `Credentials_Error` if no data was transferred.
end D_Bus.Platform;
