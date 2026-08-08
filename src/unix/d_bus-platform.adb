pragma Ada_2012;

with Ada.Directories;
with Ada.Text_IO;

with Interfaces.C.Strings;

with D_Bus.Encodings;
with System;

package body D_Bus.Platform is
   ----------------
   -- Machine ID --
   ----------------
   function Get_Machine_ID return D_Bus.Types.UUID is
      function Read_All_File (Name : String) return String;
      function Read_All_File (Name : String) return String is
         use Ada.Text_IO;
         File : File_Type;
      begin
         Open (File, In_File, Name);

         return Res : constant String := Get_Line (File) do
            Close (File);
         end return;
      end Read_All_File;
   begin
      if Ada.Directories.Exists ("/var/lib/dbus/machine-id") then
         return
           D_Bus.Types.UUID
             (D_Bus.Encodings.From_Hex
                (Read_All_File ("/var/lib/dbus/machine-id")));
      elsif Ada.Directories.Exists ("/etc/machine-id") then
         return
           D_Bus.Types.UUID
             (D_Bus.Encodings.From_Hex (Read_All_File ("/etc/machine-id")));
      else
         raise No_Machine_ID;
      end if;
   end Get_Machine_ID;

   -----------
   -- USERS --
   -----------
   function Get_User_ID return String is
      function Get_User_ID_C return Interfaces.C.Strings.chars_ptr
      with Import => True, Convention => C;
      Ptr : Interfaces.C.Strings.chars_ptr;
   begin
      Ptr := Get_User_ID_C;

      return Result : constant String := Interfaces.C.Strings.Value (Ptr) do
         Interfaces.C.Strings.Free (Ptr);
      end return;
   end Get_User_ID;

   ---------------
   -- PROCESSES --
   ---------------
   function Is_Running
     (Handle : GNATCOLL.OS.Process.Process_Handle) return Boolean
   is
      function Is_Running_C
        (Handle : GNATCOLL.OS.Process.Process_Handle)
         return Interfaces.C.C_bool
      with Import => True, Convention => C;
   begin
      return Boolean (Is_Running_C (Handle));
   end Is_Running;

   ----------------------
   -- FILE DESCRIPTORS --
   ----------------------
   Max_Possible_FDs : Integer
   with Import => True, Convention => C;

   type FD_Array is
     array (Natural range 0 .. Max_Possible_FDs)
     of GNAT.OS_Lib.File_Descriptor;
   Buf : FD_Array;

   function File_Descriptor_Passing_Support
     (S : GNAT.Sockets.Socket_Type) return Boolean
   is
      use type GNAT.Sockets.Family_Type;
   begin
      return
        GNAT.Sockets.Get_Socket_Name (S).Family = GNAT.Sockets.Family_Unix;
   end File_Descriptor_Passing_Support;

   procedure Receive_Data_With_FDs
     (Socket : GNAT.Sockets.Socket_Type;
      Item   : out Ada.Streams.Stream_Element_Array;
      Last   : out Ada.Streams.Stream_Element_Offset;
      FDs    : in out FD_Vector)
   is
      use type Interfaces.C.C_bool;

      function Receive_Data_With_FDs_C
        (Socket      : GNAT.Sockets.Socket_Type;
         Item        : System.Address;
         Item_Length : Ada.Streams.Stream_Element_Count;
         Last        : out Ada.Streams.Stream_Element_Offset;
         FDs         : System.Address;
         FD_Count    : out Integer) return Interfaces.C.C_bool
      with Convention => C, Import => True;

      FD_Count : Natural;
   begin
      if not Receive_Data_With_FDs_C
               (Socket,
                Item (Item'First)'Address,
                Item'Length,
                Last,
                Buf (Buf'First)'Address,
                FD_Count)
      then
         raise File_Descriptor_Error;
      end if;

      for I in Buf'First .. FD_Count - 1 loop
         FDs.Append (Buf (I));
      end loop;
   end Receive_Data_With_FDs;
   --  Read data from a socket and attempt to retrieve FDs transferred on it.
   --  Raise File_Descriptor_Error if file descriptors may have been lost

   procedure Send_Data_With_FDs
     (Socket : GNAT.Sockets.Socket_Type;
      Item   : Ada.Streams.Stream_Element_Array;
      FDs    : FD_Vector)
   is
      Last : Ada.Streams.Stream_Element_Offset;
   begin
      GNAT.Sockets.Send_Socket (Socket, Item, Last);
   end Send_Data_With_FDs;
   --  Write data to a socket and attempt to send FDs over it
   --  Raise File_Descriptor_Error if no file descriptors could be sent

   -----------------
   -- CREDENTIALS --
   -----------------
   function Read_Credentials (S : GNAT.Sockets.Socket_Type) return String is
      use type Interfaces.C.Strings.chars_ptr;

      function Read_Credentials_C
        (S : Integer) return Interfaces.C.Strings.chars_ptr
      with Import => True, Convention => C;

      Ptr : Interfaces.C.Strings.chars_ptr;
   begin
      Ptr := Read_Credentials_C (GNAT.Sockets.To_C (S));

      if Ptr = Interfaces.C.Strings.Null_Ptr then
         raise Credentials_Error;
      end if;

      return Result : constant String := Interfaces.C.Strings.Value (Ptr) do
         Interfaces.C.Strings.Free (Ptr);
      end return;
   end Read_Credentials;

   procedure Write_Credentials (S : GNAT.Sockets.Socket_Type) is
      use type Interfaces.C.C_bool;

      function Write_Credentials_C (S : Integer) return Interfaces.C.C_bool
      with Import => True, Convention => C;
   begin
      if not Write_Credentials_C (GNAT.Sockets.To_C (S)) then
         raise Credentials_Error;
      end if;
   end Write_Credentials;
end D_Bus.Platform;
