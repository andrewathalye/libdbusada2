pragma Ada_2012;

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Exceptions;
with Ada.Containers.Doubly_Linked_Lists;
with Ada.Text_IO;

with Interfaces;

package body D_Bus.Connection is
   ------------------
   -- GUID Storage --
   ------------------
   type GUID_String is new String (1 .. 32);
   package GUID_Lists is new Ada.Containers.Doubly_Linked_Lists
     (Natural, GUID_String);
   subtype GUID_List is GUID_Lists.List;

   Global_GUIDs : GUID_List;
   --  TODO thread safety?

   ------------
   -- Stream --
   ------------
   overriding procedure Read
     (Stream : in out Alignable_Stream;
      Item   : out Ada.Streams.Stream_Element_Array;
      Last   : out Ada.Streams.Stream_Element_Offset)
   is
   begin
   end Read;

   overriding procedure Write
     (Stream : in out Alignable_Stream;
      Item   : Ada.Streams.Stream_Element_Array);

   -----------------------------
   -- Authentication Commands --
   -----------------------------
   type SASL_Operand is (O_Auth, O_Cancel, O_Begin, O_Data, O_Error, O_Negotiate_Unix_Fd, O_Rejected, O_Ok, O_Agree_Unix_Fd);

   procedure SASL_Send
     (C : Connection; Operand : Operand_Type; Message : String := "")
   is
      OS : constant String := Operand'Image;
   begin
      --  Write operand
      String'Write (C.Stream, OS (OS'First + 2 .. OS'Last));

      --  Append message
      if Message'Length > 0 then
         Character'Write (C.Stream, ' ');
         String'Write (C.Stream, Message);
      end if;

      --  Line ending
      String'Write (C.Stream, ASCII.CR & ASCII.LF);
   end SASL_Send;

   function SASL_Receive (C : Connection; Message : out Unbounded_String) return Operand_Type is
      Char : Character;
      Message_Index : Natural := 0;
      Internal_Buffer : Unbounded_String;
   begin
      while Char /= ASCII.CR loop
         Char := Character'Read (C.Stream);
         Internal_Buffer.Append (Char);
      end loop;

      Discard := Char'Read (C.Stream); --  Discard LF

      declare
         Internal_Buffer_String : constant String := To_String (Internal_Buffer);
         Operand : Operand_Type;
      begin
         --  Find operand
         for I in Internal_Buffer_String'Range loop
            if Internal_Buffer_String (I) = ' ' then
               Operand := Operand_Type'Value ("Op_" & Internal_Buffer_String (Internal_Buffer_String'First .. (I - 1)));
               Message := To_Unbounded_String (Internal_Buffer_String (I + 1 .. Internal_Buffer_String'Last));
               exit;
            end if;
         end loop;

         return Operand;
      end;
   end SASL_Receive;

   ---------------
   -- Connected --
   ---------------
   function Connected (C : Connection) return Boolean is
   begin
      return C.Connected;
   end Connected;

   procedure Destroy (C : in out Connection) is
   begin
      GNAT.Sockets.Free (C.Stream);
      GNAT.Sockets.Close_Socket (C.Socket);
   end Destroy;

   ----------------
   -- Disconnect --
   ----------------

   procedure Disconnect (C : in out Connected_Connection) is
   begin
      C.Connected := False;

      Destroy (C);
   end Disconnect;

   -------------
   -- Connect --
   -------------
   procedure Connect
     (C    : in out Disconnected_Connection; Address : String)
   is
   begin
      --  Connect socket, create stream
      GNAT.Sockets.Connect_Socket (C.Socket, Parse (Address));
      C.Stream := new Alignable_Stream'(Socket => C.Socket);

      Authenticate :
      declare
         GUID : GUID_String;
      begin
         GUID := Auth (C.Stream);

         if Global_GUIDs.Contains (GUID) then
            Write (C, Cancel);
            Destroy (C);

            raise Duplicate_Connection with "A connection already exists to a server with UUID " & GUID'Image;
         end if;
      end Authenticate;
      
      --  Check for FD support
      Write (C, Op_Negotiate_Unix_Fd);
      case Read (C, Buffer) is
         when Op_Agree_Unix_Fd =>
            C.Unix_Fd_Support := True;
         when Op_Error =>
            Destroy (C);
            raise Connection_Error with To_String (Buffer);
         when others =>
            Destroy (C);
            raise Protocol_Error;
      end case;

      --  Begin message stream
      Write (C, Op_Begin);

      C.Connected := True;
   end Connect;

   ------------
   -- Listen --
   ------------

   procedure Listen
     (C    : in out Disconnected_Connection; Address : String)
   is
   begin
      --  Bind socket and listen
      GNAT.Sockets.Bind_Socket (C.Socket, Parse (Address));
      GNAT.Sockets.Listen_Socket (C.Socket);

      --  Authentication state loop
      Authenticate :
      loop
         begin
            Auth (C.Stream);
         exception
            when X : Authentication_Error | Authentication_Rejected =>
               Error_Message
                 (Ada.Exceptions.Exception_Name (X) & Ada.Exceptions.Exception_Message (X));
               raise Authentication_Aborted;
         end;
      end loop Authenticate;

      --  Send OK with GUID
      Write (C, Ok, String (Generate_UUID));

      --  State loop post OK
      loop
         case Read (C) is
            when A_Begin => exit;
            when Negotiate_Unix_Fd =>
               C.Unix_Fd_Support := True;
               Write (C, Agree_Unix_Fd);
            when Cancel =>
               raise Authentication_Aborted;
         end case;
      end loop;

      C.Connected := True;
   exception
      when Authentication_Aborted =>
         Destroy_Connection (C);
   end Listen;

end D_Bus.Connection;
