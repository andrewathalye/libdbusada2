pragma Ada_2012;

with Ada.Unchecked_Conversion;

with GNAT.Regexp;

with D_Bus.Types;
use type D_Bus.Types.Variant;

package body D_Bus.Messages is
   -------------------
   -- Message Flags --
   -------------------
   type MF_Ersatz_T is mod 2 ** 8;
   for MF_Ersatz_T'Size use 8;
   function Convert is new Ada.Unchecked_Conversion
     (MF_Ersatz_T, Message_Flags);
   function Convert is new Ada.Unchecked_Conversion
     (Message_Flags, MF_Ersatz_T);

   procedure Read
     (Stream :     not null access Ada.Streams.Root_Stream_Type'Class;
      Item   : out Message_Flags)
   is
      MF_Ersatz : MF_Ersatz_T;
   begin
      MF_Ersatz_T'Read (Stream, MF_Ersatz);
      Item := Convert (MF_Ersatz);
   end Read;

   procedure Write
     (Stream : not null access Ada.Streams.Root_Stream_Type'Class;
      Item   : Message_Flags)
   is
      MF_Ersatz : MF_Ersatz_T;
   begin
      MF_Ersatz := Convert (Item);
      MF_Ersatz_T'Write (Stream, MF_Ersatz);
   end Write;

   --------------------
   -- Serial Numbers --
   --------------------
   Current_Serial : Interfaces.Unsigned_32 := 1;
   function Next_Serial return D_Bus.Types.Uint32;
   function Next_Serial return D_Bus.Types.Uint32 is
   begin
      Current_Serial := Current_Serial + 1;
      return +Current_Serial;
   end Next_Serial;

   -----------------------
   -- Validity Checking --
   -----------------------
   --  TODO enforce length restriction too
   Interface_Regex : constant GNAT.Regexp.Regexp := GNAT.Regexp.Compile
     (Pattern => "^([a-zA-Z_]([a-zA-Z0-9_])*\.)+[a-zA-Z_]([a-zA-Z0-9_])*",
      Glob => True,
      Case_Sensitive => True);

   Member_Regex : constant GNAT.Regexp.Regexp := GNAT.Regexp.Compile
     (Pattern => "^([a-zA-Z0-9_])*$",
      Glob => True,
      Case_Sensitive => True);

   --  TODO this pattern needs to be checked
   Bus_Regex : constant GNAT.Regexp.Regexp := GNAT.Regexp.Compile
     (Pattern =>
         "^((:[A-Za-z0-9])|([A-Za-z]))(\.[a-zA-Z0-9]([a-zA-Z0-9_-])*)*$",
      Glob => True,
      Case_Sensitive => True);

   function Valid_Interface (X : String) return Boolean is
   begin
      return GNAT.Regexp.Match (X, Interface_Regex);
   end Valid_Interface;

   function Valid_Member (X : String) return Boolean is
   begin
      return GNAT.Regexp.Match (X, Member_Regex);
   end Valid_Member;

   function Valid_Bus (X : String) return Boolean is
   begin
      return GNAT.Regexp.Match (X, Bus_Regex);
   end Valid_Bus;

   function Valid_Message (M : U_Message) return Boolean is
      use type Ada.Containers.Count_Type;
   begin
      if not M.Valid then
         goto Fail;
      end if;

      if +M.Body_Length = 0 and M.Arguments.Length /= 0 then
         goto Fail;
      elsif +M.Body_Length /= 0 and M.Arguments.Length = 0 then
         goto Fail;
      end if;

      --  TODO detailed field checking?

      return True;
      <<Fail>>
      return False;
   end Valid_Message;

   ------------------
   -- Compose_Call --
   ------------------
   function Compose_Call
     (Flags  : Message_Flags := Default_Message_Flags;
      Path   : D_Bus.Types.Object_Path; M_Interface : Interface_Name := +"";
      Member : Member_Name; Destination : Bus_Name := +"") return Message
   is
      Fields : Field_Map;
   begin
      Fields.Insert (F_Path, +Path);

      if M_Interface.Length /= 0 then
         Fields.Insert (F_Interface, +M_Interface);
      end if;

      Fields.Insert (F_Member, +Member);

      if Destination.Length /= 0 then
         Fields.Insert (F_Destination, +Destination);
      end if;

      return U_Message'
               (Valid            => True,
                Endianness       => <>,
                M_Type           => Method_Call,
                Flags            => Flags,
                Protocol_Version => <>,
                Body_Length      => <>,
                Serial           => Next_Serial,
                Fields           => Fields,
                Arguments        => <>);
   end Compose_Call;

   --------------------
   -- Compose_Return --
   --------------------
   function Compose_Return
     (Flags       : Message_Flags := Default_Message_Flags; Reply_To : Message;
      Destination : Bus_Name      := +"") return Message
   is
      Fields : Field_Map;
   begin
      Fields.Insert (F_Reply_Serial, +Reply_To.Serial);
      if Destination.Length /= 0 then
         Fields.Insert (F_Destination, +Destination);
      end if;

      return U_Message'
        (Valid            => True,
         Endianness       => <>,
         M_Type           => Method_Return,
         Flags            => Flags,
         Protocol_Version => <>,
         Body_Length      => <>,
         Serial           => Next_Serial,
         Fields           => Fields,
         Arguments        => <>);
   end Compose_Return;

   -------------------
   -- Compose_Error --
   -------------------
   function Compose_Error
     (Flags    : Message_Flags := Default_Message_Flags; Error : Error_Name;
      Reply_To : Message; Destination : Bus_Name := +"") return Message
   is
      Fields : Field_Map;
   begin
      Fields.Insert (F_Error_Name, +Error);
      Fields.Insert (F_Reply_Serial, +Reply_To.Serial);
      if Destination.Length /= 0 then
         Fields.Insert (F_Destination, +Destination);
      end if;

      return U_Message'
               (Valid            => True,
                Endianness       => <>,
                M_Type           => D_Bus.Messages.Error,
                Flags            => Flags,
                Protocol_Version => <>,
                Body_Length      => <>,
                Serial           => Next_Serial,
                Fields           => Fields,
                Arguments        => <>);
   end Compose_Error;

   --------------------
   -- Compose_Signal --
   --------------------
   function Compose_Signal
     (Flags  : Message_Flags := Default_Message_Flags;
      Path   : D_Bus.Types.Object_Path; M_Interface : Interface_Name;
      Member : Member_Name) return Message
   is
      Fields : Field_Map;
   begin
      Fields.Insert (F_Path, +Path);
      Fields.Insert (F_Interface, +M_Interface);
      Fields.Insert (F_Member, +Member);

      return U_Message'
               (Valid            => True,
                Endianness       => <>,
                M_Type           => Signal,
                Flags            => Flags,
                Protocol_Version => <>,
                Body_Length      => <>,
                Serial           => Next_Serial,
                Fields           => Fields,
                Arguments        => <>);
   end Compose_Signal;

   -------------------
   -- Add_Arguments --
   -------------------
   procedure Add_Arguments
     (M : out Message; Arguments : D_Bus.Types.Argument_List)
   is
      use type D_Bus.Types.D_Signature;
      Length : Interfaces.Unsigned_32 := 0;
   begin
      for A of Arguments loop
         M.Arguments.Append (A);
         Length := Length + A'Size;
         --  TODO this is a tough one. not sure how to implement atm
         --  in memory size and size with padding will be different
      end loop;

      --  Recalculate signature and body length
      M.Fields.Insert
        (F_Signature,
         D_Bus.Types.Variant'(
            +D_Bus.Types.D_Signature'(
               +D_Bus.Types.Signature
                  (M.Arguments))));
      M.Body_Length := +(+M.Body_Length + Length);
   end Add_Arguments;

   ------------
   -- M_Type --
   ------------
   function M_Type (M : Message) return Message_Type is
   begin
      return M.M_Type;
   end M_Type;

   ----------
   -- Path --
   ----------
   function Path (M : Message) return D_Bus.Types.Object_Path is
      use D_Bus.Types;
   begin
      return D_Bus.Types.Object_Path (Get (M.Fields (F_Path)));
   end Path;

   -----------------
   -- M_Interface --
   -----------------
   function M_Interface (M : Message) return Interface_Name is
   begin
      return Interface_Name (+M.Fields (F_Interface));
   end M_Interface;

   ------------
   -- Member --
   ------------
   function Member (M : Message) return Member_Name is
   begin
      return Member_Name (+M.Fields (F_Member));
   end Member;

   -----------
   -- Error --
   -----------
   function Error (M : Message) return Error_Name is
   begin
      return Error_Name (+M.Fields (F_Error_Name));
   end Error;

   --------------
   -- Is_Reply --
   --------------
   function Is_Reply (Original, Reply : Message) return Boolean is
   begin
      return Original.Serial = Message_Serial
        (+Reply.Fields (F_Error_Name));
   end Is_Reply;

   -----------------
   -- Destination --
   -----------------
   function Destination (M : Message) return Bus_Name is
   begin
      return Bus_Name (+M.Fields (F_Destination));
   end Destination;

   ------------
   -- Sender --
   ------------
   function Sender (M : Message) return Bus_Name is
   begin
      return Bus_Name (+M.Fields (F_Sender));
   end Sender;

   ---------------
   -- Signature --
   ---------------
   function Signature (M : Message) return D_Bus.Types.D_Signature is
   begin
      return D_Bus.Types.D_Signature (+M.Fields (F_Signature));
   end Signature;

   ---------------
   -- Arguments --
   ---------------
   function Arguments (M : Message) return D_Bus.Types.Argument_List is
   begin
      return M.Arguments;
   end Arguments;

   -------------------
   -- U_Message I/O --
   -------------------
   procedure Read
     (Stream :     not null access Ada.Streams.Root_Stream_Type'Class;
      Item   : out U_Message)
   is
   begin
      --  TODO ideally don’t use manually subprogram
      Message_Endianness'Read (Stream, Item.Endianness);
      Message_Type'Read (Stream, Item.M_Type);
      Message_Flags'Read (Stream, Item.Flags);
      D_Bus.Types.Byte'Read (Stream, Item.Protocol_Version);
      D_Bus.Types.Uint32'Read (Stream, Item.Body_Length);
      Message_Serial'Read (Stream, Item.Serial);

      --  TODO continue deserialisation
      --  Read field map as dict (a{yv})
      Field_Map'Read (Stream, Item.Fields);

      --  TODO not really sure here
      D_Bus.Types.Argument_List'Read (Stream, Item.Arguments);
   end Read;

   procedure Write
     (Stream : not null access Ada.Streams.Root_Stream_Type'Class;
      Item   : U_Message)
   is
      pragma Unreferenced (Stream);
      pragma Unreferenced (Item);
   begin
      pragma Warnings (Off);
      raise Program_Error with "Unimplemented procedure Write";
      pragma Warnings (On);
   end Write;

end D_Bus.Messages;
