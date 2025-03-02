pragma Ada_2012;

with Ada.Directories;
with Ada.Text_IO;

with Interfaces.C.Strings;

with D_Bus.Encodings;
with D_Bus.Logging; use D_Bus.Logging;

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
      function Get_User_ID_C return Interfaces.C.Strings.chars_ptr with
        Import => True, Convention => C;
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
         return Interfaces.C.C_bool with
        Import => True, Convention => C;
   begin
      return Boolean (Is_Running_C (Handle));
   end Is_Running;

   ----------------------
   -- FILE DESCRIPTORS --
   ----------------------
   Max_Possible_FDs : Integer with
     Import => True, Convention => C;

   function File_Descriptor_Passing_Support
     (S : GNAT.Sockets.Socket_Type) return Boolean
   is
      use type GNAT.Sockets.Family_Type;
   begin
      return
        GNAT.Sockets.Get_Socket_Name (S).Family = GNAT.Sockets.Family_Unix;
   end File_Descriptor_Passing_Support;

   package body File_Descriptor_Passing is
      function Read_FDs
        (Socket : GNAT.Sockets.Socket_Type; Token : out Token_Type)
         return File_Descriptor_Array
      is
         type Result_Type is (Destructive, Transient, Success) with
           Convention => C;

         Buf      : aliased File_Descriptor_Array (1 .. Max_Possible_FDs);
         FD_Count : Integer;

         function Read_FDs_C
           (Socket       : Integer; FDs : access GNAT.OS_Lib.File_Descriptor;
            FD_Count     : out Integer; Token : out Token_Type;
            Token_Length :     Integer) return Result_Type with
           Import => True, Convention => C;
      begin
         Log (Info, "Read FDs");

         --  TODO what happens if fds length is zero?
         case Read_FDs_C
           (Socket => GNAT.Sockets.To_C (Socket),
            FDs    => Buf (Buf'First)'Access, FD_Count => FD_Count,
            Token  => Token, Token_Length => Token'Size / 8)
         is
            when Destructive =>
               raise File_Descriptor_Destructive_Error;
            when Transient =>
               raise File_Descriptor_Error;
            when Success =>
               return Buf (1 .. FD_Count);
         end case;
      end Read_FDs;

      procedure Write_FDs
        (Socket : GNAT.Sockets.Socket_Type; FDs : File_Descriptor_Array;
         Token  : Token_Type)
      is
         use type Interfaces.C.C_bool;

         function Write_FDs_C
           (Socket       : Integer;
            FDs          : access constant GNAT.OS_Lib.File_Descriptor;
            FD_Count     : Integer; Token : access constant Token_Type;
            Token_Length : Integer) return Interfaces.C.C_bool with
           Import => True, Convention => C;

         Aliased_Token : aliased constant Token_Type := Token;
      begin
         Log (Info, "Write FDs");

         --  TODO what happens if fds length is zero?
         if not Write_FDs_C
             (Socket => GNAT.Sockets.To_C (Socket),
              FDs    => FDs (FDs'First)'Access, FD_Count => FDs'Length,
              Token  => Aliased_Token'Access, Token_Length => Token'Size / 8)
         then
            raise File_Descriptor_Error;
         end if;
      end Write_FDs;
   end File_Descriptor_Passing;

   -----------------
   -- CREDENTIALS --
   -----------------
   function Read_Credentials (S : GNAT.Sockets.Socket_Type) return String is
      use type Interfaces.C.Strings.chars_ptr;

      function Read_Credentials_C
        (S : Integer) return Interfaces.C.Strings.chars_ptr with
        Import => True, Convention => C;

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

      function Write_Credentials_C
        (S : Integer) return Interfaces.C.C_bool with
        Import => True, Convention => C;
   begin
      if not Write_Credentials_C (GNAT.Sockets.To_C (S)) then
         raise Credentials_Error;
      end if;
   end Write_Credentials;
end D_Bus.Platform;
