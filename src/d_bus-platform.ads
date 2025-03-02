pragma Ada_2012;

with GNAT.OS_Lib;
with GNAT.Sockets;
with GNATCOLL.OS.Process;

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
   File_Descriptor_Error             : exception;
   File_Descriptor_Destructive_Error : exception;
   type File_Descriptor_Array is
     array (Positive range <>) of aliased GNAT.OS_Lib.File_Descriptor;

   function File_Descriptor_Passing_Support
     (S : GNAT.Sockets.Socket_Type) return Boolean;
   --  Returns whether socket `S` supports transferring file descriptors
   --  on the current platform.
   generic
      type Token_Type is (<>);
   package File_Descriptor_Passing is
      --  Some platforms require that file descriptors be passed alongside
      --  some other data. It is safe to pass a null record or empty type
      --  alongside `Token_Type` iff your platform does not require data
      --  to be transferred alongside file descriptors.
      --
      --  Because `Token_Type` may be used by an implementation unaware
      --  of Ada conventions, it must be a discrete type (ARM) and the
      --  user must not assume that representation pragmas will be observed.

      function Read_FDs
        (Socket : GNAT.Sockets.Socket_Type; Token : out Token_Type)
         return File_Descriptor_Array;
      --  Read an array of file descriptors from a socket.
      --  Consumes `Token` from the socket.
      --
      --  On failure:
      --  Raise `File_Descriptor_Error` if no data was consumed.
      --  Raise `File_Descriptor_Destructive_Error` if data was consumed.

      procedure Write_FDs
        (Socket : GNAT.Sockets.Socket_Type; FDs : File_Descriptor_Array;
         Token  : Token_Type);
      --  Pass an array of file descriptors over a socket.
      --  Writes `Token` to the socket.
      --
      --  On failure:
      --  Raise `File_Descriptor_Error` if no data was transferred.
   end File_Descriptor_Passing;

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
