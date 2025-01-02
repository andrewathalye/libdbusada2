pragma Ada_2012;

with Ada.IO_Exceptions;
with Ada.Strings.Unbounded;
with Ada.Strings.Fixed;
with Ada.Exceptions;
with Ada.Containers.Doubly_Linked_Lists;
with Ada.Text_IO;
with Ada.Text_IO.Modular_IO;

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

   --------------------
   -- Address Parser --
   --------------------
   function Parse (S : String) return GNAT.Sockets.Sock_Addr_Type is
   begin
      raise Program_Error with "Unimplemented";
   end Parse;

   ------------
   -- Stream --
   ------------
   overriding procedure Read
     (Stream : in out Alignable_Stream;
      Item   : out Ada.Streams.Stream_Element_Array;
      Last   : out Ada.Streams.Stream_Element_Offset)
   is
   begin
      GNAT.Sockets.Receive_Socket (
        (Socket => Stream.Socket,
         Item => Item,
         Last => Last);

      Stream.Read_Count := Stream.Read_Count + Last; 
   end Read;

   overriding procedure Write
     (Stream : in out Alignable_Stream;
      Item   : Ada.Streams.Stream_Element_Array)
   is
      use type Ada.Streams.Stream_Element_Offset;
      Last : Ada.Streams.Stream_Element_Offset;
   begin
      GNAT.Sockets.Send_Socket
        (Socket => Stream.Socket,
         Item => Item,
         Last => Last);

      Stream.Write_Count := Stream.Write_Count + Last;

      if Last /= Item'Length then
         raise Ada.IO_Exceptions.End_Error;
      end if;
   end Write;

   -----------------------------
   -- Authentication Protocol --
   -----------------------------
   type SASL_State is (S_Begin, S_Auth, S_Unix_Fd_Support, S_End, S_Error);
   type SASL_Operand is (O_Auth, O_Cancel, O_Begin, O_Data, O_Error, O_Negotiate_Unix_Fd, O_Rejected, O_Ok, O_Agree_Unix_Fd);
   type SASL_Method is (M_External, M_DBus_Cookie_SHA1, M_Anonymous);

   package SASL_Method_Lists is new Ada.Containers.Doubly_Linked_Lists (SASL_Method);
   subtype SASL_Method_List is SASL_Method_Lists.List;

   function SASL_Chop (S : String) return String is (S (S'First + 2 .. S'Last));
   --  Chop up enum names

   package U8_IO is new Ada.Text_IO.Modular_IO (U8);

   function SASL_Hex (S : String) return String
   is
      Buf : String (1 .. S'Length * 2);
      Short_Buf : String (1 .. 2);
   begin
      for I in S'Range loop
         U8_IO.Put (Short_Buf, Character'Pos (S'I));
      end loop;

      return Buf;
   end SASL_Hex;

   function SASL_Unhex (S : String) return String
   is
      Buf : String (1 .. S'Length / 2); --  Half as many elements in raw string
      U8 : Interfaces.Unsigned_8;
      Discard : Positive;
   begin

      if S'Length mod 2 /= 0 then
         raise Protocol_Error with "hex string length must be even";
      end if;

      for I in S'Range loop
         U8_IO.Get (S (I .. I + 1), U8, Discard);
         Buf (I / 2) := Character'Val (U8);
         I := I + 1;
      end loop;

      return Buf;
   end SASL_Unhex;

   procedure SASL_Send
     (C : Connection; Operand : Operand_Type; Message : String := "")
   is
   begin
      --  Write operand
      String'Write (C.Stream, SASL_Chop (Operand'Image));

      --  Append message
      if Message'Length > 0 then
         Character'Write (C.Stream, ' ');
         String'Write (C.Stream, Message);
      end if;

      --  Line ending
      String'Write (C.Stream, ASCII.CR & ASCII.LF);
   end SASL_Send;

   function SASL_Receive (C : Connection; Message : out Unbounded_String) return Operand_Type is
      use Ada.Strings.Unbounded;

      Char : Character;
      Message_Index : Integer;
      Internal_Buffer : Unbounded_String;
   begin
      while Char /= ASCII.CR loop
         Char := Character'Read (C.Stream);
         Internal_Buffer.Append (Char);
      end loop;

      Char := Character'Read (C.Stream); --  Discard LF

      Find_Message_Index :
      begin
         Message_Index := Index (Internal_Buffer, " ");
       exception
            when Index_Error =>
               Message_Index := Length (Internal_Buffer);
               --  TODO test if this is what we actually want
      end;

      Message := Unbounded_Slice (Internal_Buffer, Message_Index, Length (Internal_Buffer));

      return SASL_Operand'Value ("O_" & Slice (Internal_Buffer, Message_Index));
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

   procedure Client_Auth
     (C : U_Connection;
      State : out SASL_State;
      Error : out Ada.Strings.Unbounded.Unbounded_String);

   procedure Client_Auth
     (C : U_Connection;
      State : out SASL_State;
      Error : out Ada.Strings.Unbounded.Unbounded_String)
   is
      use Ada.Strings.Unbounded.Unbounded_String;
      Message : Ada.Strings.Unbounded.Unbounded_String;
      Methods : SASL_Method_List;
   begin
      --  Send empty auth command. It must be rejected.
      SASL_Send (C, O_Auth);
      case SASL_Receive (C, Message) is
         when O_Rejected => null;
         when others => raise Protocol_Error;
      end case;

      --  Parse response and find common methods
      Common_Methods :
      declare
         Current_Index : Natural := 1;
      begin
         loop
            --  Next index of a " " character
            Index := Index (Message, " ", Index + 1);

            Try_Add_Method :
            declare
               Method_Name : constant String := Slice (Message, 1, Index - 1);
            begin
               Ada.Text_IO.Put_Line (Method_Name);
               Message := Unbounded_Slice (Message, Index + 1, Length (Message));
               Methods.Append (SASL_Method'Value ("M_" & Method_Name));
            exception
               when Constraint_Error => null;
            end Try_Add_Method;
         end loop;
      exception
         when Index_Error => null;
         --  Stop searching when there are no more spaces
      end Common_Methods;

      if Methods.Is_Empty then
         State := S_Error;
         Error := To_Unbounded_String ("No supported authentication methods");
         goto Auth_Over;
      end if;

      Try_All_Methods :
      for Method of Methods loop
         case Method is
            when M_External =>
               --  TODO send creds
               SASL_Send
                 (C => C,
                  Operand => O_Auth,
                  Message =>
                     SASL_Chop (M_External'Image)
                        & " " & To_Hex (Get_UID'Image));

               goto Auth_Check_Result;
            when M_DBus_Cookie_SHA1 =>
               SASL_Send
                 (C => C,
                  Operand => O_Auth,
                  Message =>
                     SASL_Chop (M_DBus_Cookie_SHA1'Image)
                     & " " & To_Hex (Get_Username));

               --  TODO implement better
               pragma Assert (SASL_Receive (Message) = O_Data);
               Ada.Text_IO.Put_Line (To_String (Message));
               --  Unhex
               --  Cookie_Context " " Secret Cookie Integer ID " " random string
               --  Compute server challenge ":" client challenge ":" cookie
               --  Sha1 digest -> hex
               --  Compute client challenge " " sha1)
               --  Hex
               raise Program_Error;

            when M_Anonymous =>
               SASL_Send (C, O_Auth, SASL_Chop (M_Anonymous'Image));
               goto Auth_Check_Result;
         end case;

         raise Program_Error;
         <<Auth_Check_Result>>
         case SASL_Receive (C, Message) is
            when O_Ok =>
               goto Auth_Check_GUID;
            when O_Rejected =>
               goto Auth_Next_Method;
            when O_Error =>
               S := S_Error;
               Error := Message;
               goto Auth_Over;
         end case;

         raise Program_Error;
         <<Auth_Next_Method>>
      end loop Try_All_Methods;

      raise Program_Error;
      <<Auth_Check_GUID>>
      --  `Message` must contain the server GUID
      if Global_GUIDS.Contains
        (GUID_String (To_String (Message)))
      then
         --  TODO
         for GUID of Global_GUIDs loop
            Ada.Text_IO.Put_Line (String (GUID));
         end loop;

         State := S_Error;
         Error := To_Unbounded_String ("Duplicate GUID");
         goto Auth_Over;
      end if;
      Global_GUIDs.Append (GUID_String (To_String (Message)));
      goto Auth_Success;

      raise Program_Error;
      <<Auth_Success>>
      State := S_Unix_Fd_Support;
      goto Auth_Over;

      raise Program_Error;
      <<Auth_Over>>
   end Client_Auth;

   -------------
   -- Connect --
   -------------
   procedure Connect
     (C    : in out Disconnected_Connection; Address : String)
   is
      use Ada.Strings.Unbounded;
   begin
      --  Connect socket, create stream
      GNAT.Sockets.Connect_Socket (C.Socket, Parse (Address));
      C.Stream := new Alignable_Stream'(Socket => C.Socket);

      --  State machine
      State_Machine :
      declare
         State : SASL_State := S_Begin;
         Error : Ada.Strings.Unbounded.Unbounded_String;
         Message : Ada.Strings.Unbounded.Unbounded_String;
      begin
         State_Machine_Loop :
         loop
            case State is
               when S_Begin =>
                  --  Write initial null byte
                  Character'Write (C.Stream, ASCII.NUL);

                  State := S_Auth;

               when S_Auth =>
                  Client_Auth (C, State, Error);
     
               when S_Unix_Fd_Support =>
                  SASL_Send (C, O_Negotiate_Unix_Fd);
                  case SASL_Receive (C, Message) is
                     when O_Agree_Unix_Fd =>
                        C.Unix_Fd_Support := True;
                     when O_Error =>
                        C.Unix_Fd_Support := False;
                  end case;

               when S_End =>
                 SASL_Send (C, O_Begin);
                 exit;

               when S_Error =>
                  Destroy (C);
                  raise Connection_Error
                     with Ada.Strings.Unbounded.To_String (Error);
         end loop State_Machine_Loop;
      end State_Machine;

      C.Connected := True;
   end Connect;

   ------------
   -- Listen --
   ------------
   procedure Listen
     (C    : in out Disconnected_Connection; Address : String)
   is
   begin
      raise Program_Error with "Unimplemented";
   end Listen;
begin
      U8_IO.Default_Base := 16; 
end D_Bus.Connection;
