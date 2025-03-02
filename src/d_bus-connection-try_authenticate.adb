pragma Ada_2012;

with Ada.Strings.Fixed;
with Ada.Strings.Unbounded;
with Ada.Unchecked_Conversion;

with D_Bus.Encodings;
with D_Bus.Platform;
with D_Bus.Logging; use D_Bus.Logging;

function D_Bus.Connection.Try_Authenticate
  (Mode : Mode_Type; C : in out Connection) return Boolean
is
   -----------
   -- Types --
   -----------
   type SASL_Command is
     (C_Auth, C_Cancel, C_Begin, C_Data, C_Error, C_Negotiate_Unix_Fd, C_Ok,
      C_Rejected, C_Agree_Unix_Fd);
   type SASL_State is (Initial, Authenticate, Ok, Unix_FD, Final, Failure);
   type SASL_Method is (External, Anonymous);
   --  Note: We do not support DBus_Cookie_SHA1

   -------------
   -- Utility --
   -------------
   Line_Terminator : constant String := ASCII.CR & ASCII.LF;

   procedure SASL_Send (Cmd : SASL_Command; Msg : String := "");
   procedure SASL_Send (Cmd : SASL_Command; Msg : String := "") is
      use type Ada.Streams.Stream_Element_Offset;

      function Convert is new Ada.Unchecked_Conversion
        (Character, Ada.Streams.Stream_Element);

      --------------
      -- Constant --
      --------------
      Cmd_Image   : constant String := Cmd'Image;
      Textual_Cmd :
        String renames Cmd_Image (Cmd_Image'First + 2 .. Cmd_Image'Last);
      Buffer : constant String := Textual_Cmd & " " & Msg & Line_Terminator;

      --------------
      -- Variable --
      --------------
      Last : Ada.Streams.Stream_Element_Offset;

      SEA_Buffer :
        Ada.Streams.Stream_Element_Array
          (Ada.Streams.Stream_Element_Offset (Buffer'First) ..
               Ada.Streams.Stream_Element_Offset (Buffer'Last));
   begin
      Log (Info, Buffer);

      for I in Buffer'Range loop
         SEA_Buffer (Ada.Streams.Stream_Element_Offset (I)) :=
           Convert (Buffer (I));
      end loop;

      GNAT.Sockets.Send_Socket (C.Socket, SEA_Buffer, Last);

      if Last /= SEA_Buffer'Last then
         raise Protocol_Error;
      end if;
   end SASL_Send;

   function SASL_Receive
     (Msg : out Ada.Strings.Unbounded.Unbounded_String) return SASL_Command;
   function SASL_Receive
     (Msg : out Ada.Strings.Unbounded.Unbounded_String) return SASL_Command
   is
      use type Ada.Streams.Stream_Element_Offset;
      use Ada.Strings.Unbounded;

      function Convert is new Ada.Unchecked_Conversion
        (Ada.Streams.Stream_Element, Character);

      subtype Buffer_SEA is Ada.Streams.Stream_Element_Array (1 .. 512);

      Buffer : Buffer_SEA;
      Last   : Ada.Streams.Stream_Element_Offset;
   begin
      GNAT.Sockets.Receive_Socket
        (Socket => C.Socket, Item => Buffer, Last => Last);

      --  We don’t accept messages at buffer length
      --  or messages that are too short (<2) for CR LF
      if Last = Buffer'Last or Last < 2 then
         raise Protocol_Error;
      end if;

      declare
         String_View : String (1 .. Natural (Last) - 2);
         Command_End : Natural;
      begin
         for I in 1 .. Last - 2 loop
            String_View (Natural (I)) := Convert (Buffer (I));
         end loop;

         Log (Info, String_View);

         --  Split command and parameters
         Command_End := Ada.Strings.Fixed.Index (String_View, " ");

         --  This would mean that the whole command is " "
         if Command_End = 1 then
            raise Protocol_Error;
         end if;

         --  If no " " was detected, there are no parameters
         if Command_End = 0 then
            Command_End := String_View'Last + 1;
            Msg         := Null_Unbounded_String;
         else
            Msg :=
              To_Unbounded_String
                (String_View (Command_End + 1 .. String_View'Last));
         end if;

         begin
            return
              SASL_Command'Value ("C_" & String_View (1 .. Command_End - 1));
         exception
            when Constraint_Error =>
               raise Protocol_Error;
         end;
      end;
   end SASL_Receive;

   procedure Null_Byte (Mode : Mode_Type);
   procedure Null_Byte (Mode : Mode_Type) is
      use type Ada.Streams.Stream_Element;
      use type Ada.Streams.Stream_Element_Offset;

      NBA  : Ada.Streams.Stream_Element_Array := (1 => 0);
      Last : Ada.Streams.Stream_Element_Offset;
   begin
      case Mode is
         when Connect =>
            GNAT.Sockets.Send_Socket (C.Socket, NBA, Last);
         when Listen =>
            GNAT.Sockets.Receive_Socket (C.Socket, NBA, Last);
      end case;

      if Last /= NBA'Last or NBA (NBA'First) /= 0 then
         raise Protocol_Error;
      end if;

   end Null_Byte;

   function Enumerate_Methods return String;
   function Enumerate_Methods return String is
      use Ada.Strings.Unbounded;

      Buf : Unbounded_String;
   begin
      for M in SASL_Method'Range loop
         Append (Buf, M'Image);

         if M /= SASL_Method'Last then
            Append (Buf, ' ');
         end if;
      end loop;

      return To_String (Buf);
   end Enumerate_Methods;

   function Parameter
     (Buf : Ada.Strings.Unbounded.Unbounded_String; Index : Positive)
      return String;
   function Parameter
     (Buf : Ada.Strings.Unbounded.Unbounded_String; Index : Positive)
      return String
   is
      use Ada.Strings.Unbounded;

      Spaces_Before : constant Natural := Index - 1;
      Space_Prior   : Natural          := 0;
      Space_After   : Natural          := 0;
   begin
      for I in 1 .. Spaces_Before loop
         Space_Prior := Space_After;
         Space_After :=
           Ada.Strings.Unbounded.Index
             (Source => Buf, Pattern => " ", From => Space_Prior + 1);
      end loop;

      if Space_After = 0 then
         Space_After := Length (Buf) + 1;
      end if;

      return Slice (Buf, Space_Prior + 1, Space_After - 1);
   end Parameter;
   --  TODO check

   -----------
   -- Logic --
   -----------
   function Server_Authenticate return Boolean;
   function Server_Authenticate return Boolean is
      use D_Bus.Encodings;
      use Ada.Strings.Unbounded;

      State : SASL_State := Initial;
      Buf   : Unbounded_String;
   begin
      Server_Loop :
      loop
         case State is
            --  Initial null byte
            when Initial =>
               Null_Byte (Listen);
               State := Authenticate;
            when Authenticate =>
               case SASL_Receive (Buf) is
                  when C_Auth =>
                     --  Retrieve request
                     declare
                        Requested_Method : SASL_Method;
                        Initial_Response : Unbounded_String;
                     begin
                        Requested_Method :=
                          SASL_Method'Value (Parameter (Buf, 1));
                        Initial_Response :=
                          To_Unbounded_String (From_Hex (Parameter (Buf, 2)));

                        case Requested_Method is
                           when External =>
                              --  TODO do something more?
                              if D_Bus.Platform.Read_Credentials (C.Socket) =
                                To_String (Initial_Response)
                              then
                                 State := Ok;
                              else
                                 State := Failure;
                              end if;
                           when Anonymous =>
                              --  TODO add security
                              State := Ok;
                        end case;
                     exception
                        when Constraint_Error                 =>
                           State := Failure;
                        when D_Bus.Platform.Credentials_Error =>
                           null;
                        when D_Bus.Encodings.Invalid_Encoding =>
                           raise Protocol_Error;
                     end;
                  when C_Cancel =>
                     State := Failure;
                  when others =>
                     raise Protocol_Error;
               end case;
            when Ok =>
               SASL_Send (C_Ok, To_Hex (String (New_UUID)));
               State := Unix_FD;
            when Unix_FD =>
               case SASL_Receive (Buf) is
                  when C_Begin =>
                     State := Final;
                  when C_Agree_Unix_Fd =>
                     if D_Bus.Platform.File_Descriptor_Passing_Support
                         (C.Socket)
                     then
                        SASL_Send (C_Agree_Unix_Fd);
                        C.Unix_Fd_Support := True;
                        State             := Final;
                     else
                        SASL_Send (C_Error, "UNIX FD Transfer unsupported");
                        C.Unix_Fd_Support := False;
                     end if;
                  when C_Cancel =>
                     State := Failure;
                  when others =>
                     raise Protocol_Error;
               end case;
            when Final =>
               return True;
            when Failure =>
               SASL_Send (C_Rejected, Enumerate_Methods);
               return False;
         end case;
      end loop Server_Loop;
   end Server_Authenticate;

   function Client_Authenticate return Boolean;
   function Client_Authenticate return Boolean is
      use D_Bus.Encodings;
      use Ada.Strings.Unbounded;

      State : SASL_State := Initial;
      Buf   : Unbounded_String;
   begin
      Client_Loop :
      loop
         case State is
            when Initial =>
               --  As per specification, try sending credentials
               --  On fail, send null byte manually
               begin
                  D_Bus.Platform.Write_Credentials (C.Socket);
               exception
                  when D_Bus.Platform.Credentials_Error =>
                     Log
                       (Info, "Unable to send credentials to remote server.");
                     Null_Byte (Connect);
               end;

               State := Authenticate;
            when Authenticate =>
               for M in SASL_Method'Range loop
                  case M is
                     when External =>
                        SASL_Send
                          (C_Auth,
                           External'Image & " " &
                           To_Hex (D_Bus.Platform.Get_User_ID));
                     when Anonymous =>
                        SASL_Send (C_Auth, Anonymous'Image);
                  end case;

                  case SASL_Receive (Buf) is
                     when C_Ok =>
                        State := Ok;
                        goto Done_Authenticate;
                     when C_Rejected =>
                        null;
                     when others =>
                        raise Protocol_Error;
                  end case;
               end loop;
               <<Done_Authenticate>>
            when Ok =>
               --  The actual 'Ok' message is checked earlier
               C.UUID := D_Bus.Types.UUID (From_Hex (To_String (Buf)));
               State  := Unix_FD;
            when Unix_FD =>
               if D_Bus.Platform.File_Descriptor_Passing_Support (C.Socket)
               then
                  SASL_Send (C_Negotiate_Unix_Fd);

                  case SASL_Receive (Buf) is
                     when C_Agree_Unix_Fd =>
                        C.Unix_Fd_Support := True;
                        State             := Final;
                     when C_Error =>
                        C.Unix_Fd_Support := False;
                        State             := Final;
                     when others =>
                        raise Protocol_Error;
                  end case;
               else
                  State := Final;
               end if;
            when Final =>
               SASL_Send (C_Begin);
               return True;
            when Failure =>
               SASL_Send (C_Cancel);
               return False;
         end case;
      end loop Client_Loop;
   end Client_Authenticate;
begin
   case Mode is
      when Connect =>
         return Client_Authenticate;
      when Listen =>
         return Server_Authenticate;
   end case;
end D_Bus.Connection.Try_Authenticate;
