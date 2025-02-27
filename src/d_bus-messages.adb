pragma Ada_2012;

with Ada.Unchecked_Conversion;

with D_Bus.Streams;
with Interfaces;
with System;

with D_Bus.Types.Basic_Generic;
with D_Bus.Types.Dispatching_Read;

package body D_Bus.Messages is
   --------------------
   -- Message Serial --
   --------------------
   protected Global_Serials is
      procedure Next_Serial (S : out Valid_Message_Serial);
   private
      Current_Serial : Valid_Message_Serial := 1;
   end Global_Serials;

   protected body Global_Serials is
      procedure Next_Serial (S : out Valid_Message_Serial) is
      begin
         S              := Current_Serial;
         Current_Serial := Current_Serial + 1;
      end Next_Serial;
   end Global_Serials;

   package D_Message_Serials is new D_Bus.Types.Basic_Generic.Discrete_Wrappers
     (Type_Code => D_Bus.Types.Uint32_CC, Inner => U_Message_Serial);
   subtype D_Message_Serial is D_Message_Serials.Outer;

   -------------------
   -- Message Flags --
   -------------------
   type Message_Flags_Ersatz is mod 2**8;
   for Message_Flags_Ersatz'Size use 8;

   function To_Ersatz is new Ada.Unchecked_Conversion
     (Message_Flags, Message_Flags_Ersatz);
   function To_Object is new Ada.Unchecked_Conversion
     (Message_Flags_Ersatz, Message_Flags);

   procedure Read
     (Stream :     not null access Ada.Streams.Root_Stream_Type'Class;
      Item   : out Message_Flags)
   is
      Ersatz : Message_Flags_Ersatz;
   begin
      Message_Flags_Ersatz'Read (Stream, Ersatz);
      Item := To_Object (Ersatz);
   end Read;

   procedure Write
     (Stream : not null access Ada.Streams.Root_Stream_Type'Class;
      Item   : Message_Flags)
   is
   begin
      Message_Flags_Ersatz'Write (Stream, To_Ersatz (Item));
   end Write;

   procedure Assert_Or_Protocol_Error (Expr : Boolean);
   --  Assert that the expression or true or raise a protocol error

   procedure Assert_Or_Protocol_Error (Expr : Boolean) is
   begin
      if not Expr then
         raise Protocol_Error;
      end if;
   end Assert_Or_Protocol_Error;

   ------------------
   -- Compose_Call --
   ------------------
   function Compose_Call
     (Flags       : Message_Flags                    := Default_Message_Flags;
      Path        : D_Bus.Types.Basic.Object_Path;
      M_Interface : D_Bus.Types.Extra.Interface_Name := "";
      Member      : D_Bus.Types.Extra.Member_Name;
      Destination : D_Bus.Types.Extra.Bus_Name       := "") return Message
   is
      use type D_Bus.Types.Basic.D_Object_Path;
      use type D_Bus.Types.Basic.D_String;
      Fields : Field_Maps.Map;
   begin
      Fields.Insert (F_Path, +(+Path));
      Fields.Insert (F_Interface, +(+M_Interface));
      Fields.Insert (F_Member, +(+Member));

      if Destination'Length > 0 then
         Fields.Insert (F_Destination, +(+Destination));
      end if;

      return
        (Serial => <>, M_Type => Method_Call, Flags => Flags, Fields => Fields,
         Arguments => <>);
   end Compose_Call;

   --------------------
   -- Compose_Return --
   --------------------
   function Compose_Return
     (Flags       : Message_Flags := Default_Message_Flags; Reply_To : Message;
      Destination : D_Bus.Types.Extra.Bus_Name := "") return Message
   is
      use type D_Bus.Types.Basic.D_String;
      use type D_Message_Serial;
      Fields : Field_Maps.Map;
   begin
      if Reply_To.Serial = Invalid_Serial or Reply_To.Flags.No_Reply_Expected
      then
         raise No_Reply_Expected;
      end if;

      Fields.Insert (F_Reply_Serial, +(+Reply_To.Serial));

      if Destination'Length > 0 then
         Fields.Insert (F_Destination, +(+Destination));
      end if;

      return
        (Serial => <>, M_Type => Method_Return, Flags => Flags,
         Fields => Fields, Arguments => <>);
   end Compose_Return;

   -------------------
   -- Compose_Error --
   -------------------
   function Compose_Error
     (Flags       : Message_Flags              := Default_Message_Flags;
      Error       : D_Bus.Types.Extra.Error_Name; Reply_To : Message;
      Destination : D_Bus.Types.Extra.Bus_Name := "") return Message
   is
      use type D_Bus.Types.Basic.D_String;
      use type D_Message_Serial;
      Fields : Field_Maps.Map;
   begin
      if Reply_To.Serial = Invalid_Serial or Reply_To.Flags.No_Reply_Expected
      then
         raise No_Reply_Expected;
      end if;

      Fields.Insert (F_Error_Name, +(+Error));
      Fields.Insert (F_Reply_Serial, +(+Reply_To.Serial));

      if Destination'Length > 0 then
         Fields.Insert (F_Destination, +(+Destination));
      end if;

      return
        (Serial => <>, M_Type => D_Bus.Messages.Error, Flags => Flags,
         Fields => Fields, Arguments => <>);
   end Compose_Error;

   --------------------
   -- Compose_Signal --
   --------------------
   function Compose_Signal
     (Flags       : Message_Flags := Default_Message_Flags;
      Path        : D_Bus.Types.Basic.Object_Path;
      M_Interface : D_Bus.Types.Extra.Interface_Name;
      Member      : D_Bus.Types.Extra.Member_Name) return Message
   is
      use type D_Bus.Types.Basic.D_Object_Path;
      use type D_Bus.Types.Basic.D_String;
      Fields : Field_Maps.Map;
   begin
      Fields.Insert (F_Path, +(+Path));
      Fields.Insert (F_Interface, +(+M_Interface));
      Fields.Insert (F_Member, +(+Member));

      return
        (Serial    => <>, M_Type => Signal, Flags => Flags, Fields => Fields,
         Arguments => <>);
   end Compose_Signal;

   -------------------
   -- Add_Arguments --
   -------------------
   procedure Add_Arguments
     (M : in out Message; Arguments : D_Bus.Types.Argument_List)
   is
   begin
      for A of Arguments loop
         M.Arguments.Append (A);
      end loop;
   end Add_Arguments;

   ------------
   -- M_Type --
   ------------
   function M_Type (M : Message) return Message_Type is
   begin
      return M.M_Type;
   end M_Type;

   -----------
   -- Flags --
   -----------
   function Flags (M : Message) return Message_Flags is
   begin
      return M.Flags;
   end Flags;

   ------------
   -- Fields --
   ------------
   function Path (M : Message) return D_Bus.Types.Basic.Object_Path is
      use type D_Bus.Types.Basic.D_Object_Path;
   begin
      return +D_Bus.Types.Basic.D_Object_Path (M.Fields (F_Path).Get);
   exception
      when Constraint_Error =>
         raise Field_Absent;
   end Path;

   function M_Interface (M : Message) return D_Bus.Types.Extra.Interface_Name
   is
      use type D_Bus.Types.Basic.D_String;
   begin
      return +D_Bus.Types.Basic.D_String (M.Fields (F_Interface).Get);
   exception
      when Constraint_Error =>
         raise Field_Absent;
   end M_Interface;

   function Member (M : Message) return D_Bus.Types.Extra.Member_Name is
      use type D_Bus.Types.Basic.D_String;
   begin
      return +D_Bus.Types.Basic.D_String (M.Fields (F_Member).Get);
   exception
      when Constraint_Error =>
         raise Field_Absent;
   end Member;

   function Error (M : Message) return D_Bus.Types.Extra.Error_Name is
      use type D_Bus.Types.Basic.D_String;
   begin
      return +D_Bus.Types.Basic.D_String (M.Fields (F_Error_Name).Get);
   end Error;
   function Is_Reply (Original, Reply : Message) return Boolean is
      Reply_RS : Valid_Message_Serial;
   begin
      Reply_RS :=
        Valid_Message_Serial
          (D_Bus.Types.Basic.Uint32s."+"
             (D_Bus.Types.Basic.Uint32 (Reply.Fields (F_Reply_Serial).Get)));
      return
        (not Original.Flags.No_Reply_Expected)
        and then Original.Serial = Reply_RS;
   end Is_Reply;

   function Destination (M : Message) return D_Bus.Types.Extra.Bus_Name is
      use type D_Bus.Types.Basic.D_String;
   begin
      return +D_Bus.Types.Basic.D_String (M.Fields (F_Destination).Get);
   exception
      when Constraint_Error =>
         raise Field_Absent;
   end Destination;

   function Sender (M : Message) return D_Bus.Types.Extra.Bus_Name is
      use type D_Bus.Types.Basic.D_String;
   begin
      return +D_Bus.Types.Basic.D_String (M.Fields (F_Sender).Get);
   exception
      when Constraint_Error =>
         raise Field_Absent;
   end Sender;

   function Signature (M : Message) return D_Bus.Types.Contents_Signature is
      use type D_Bus.Types.Basic.D_Signature;
   begin
      return +D_Bus.Types.Basic.D_Signature (M.Fields (F_Signature).Get);
   exception
      when Constraint_Error =>
         raise Field_Absent;
   end Signature;

   ---------------
   -- Arguments --
   ---------------
   function Arguments (M : Message) return D_Bus.Types.Argument_List is
   begin
      return M.Arguments;
   end Arguments;

   -----------
   -- I / O --
   -----------
   type Message_Endianness is (Big, Little);
   for Message_Endianness'Size use 8;
   for Message_Endianness use
     (Big => Character'Pos ('B'), Little => Character'Pos ('l'));

   type ME_Table_From_Ada_T is array (System.Bit_Order) of Message_Endianness;
   ME_Table_From_Ada          : constant ME_Table_From_Ada_T := (Big, Little);
   Default_Message_Endianness : constant Message_Endianness  :=
     ME_Table_From_Ada (System.Default_Bit_Order);

   package D_Field_Types is new D_Bus.Types.Basic_Generic.Discrete_Wrappers
     (Type_Code => D_Bus.Types.Byte_CC, Inner => Field_Type);
   subtype D_Field_Type is D_Field_Types.Outer;

   Field_Struct_Contents : constant D_Bus.Types.Contents_Signature := "yv";

   subtype Field_Map_Raw is
     D_Bus.Types.Containers.D_Array (D_Bus.Types.Intern ("(yv)"));

   type Raw_Message_Header is record
      Endianness       : Message_Endianness    := Default_Message_Endianness;
      M_Type           : Message_Type;
      Flags            : Message_Flags;
      Protocol_Version : Interfaces.Unsigned_8 := Default_Protocol_Version;
      Body_Length      : D_Bus.Types.Basic.Uint32;
      Serial           : D_Message_Serial;
      Fields           : Field_Map_Raw;
   end record;

   procedure Read_RMH
     (Stream :     not null access Ada.Streams.Root_Stream_Type'Class;
      Item   : out Raw_Message_Header);
   --  Custom read procedure that checks endianness and protocol version
   for Raw_Message_Header'Read use Read_RMH;

   procedure Write_RMH
     (Stream : not null access Ada.Streams.Root_Stream_Type'Class;
      Item   : Raw_Message_Header);
   --  Custom write procedure that handles padding
   for Raw_Message_Header'Write use Write_RMH;

   procedure Read_RMH
     (Stream :     not null access Ada.Streams.Root_Stream_Type'Class;
      Item   : out Raw_Message_Header)
   is
      use type Interfaces.Unsigned_8;
   begin
      Message_Endianness'Read (Stream, Item.Endianness);
      Assert_Or_Protocol_Error (Item.Endianness = Default_Message_Endianness);
      --  TODO support other endianness values

      Message_Type'Read (Stream, Item.M_Type);
      Assert_Or_Protocol_Error (Item.M_Type /= Invalid);

      Message_Flags'Read (Stream, Item.Flags);

      Interfaces.Unsigned_8'Read (Stream, Item.Protocol_Version);
      Assert_Or_Protocol_Error
        (Item.Protocol_Version = Default_Protocol_Version);

      --  All fields from here on out depend on endianness
      D_Bus.Types.Basic.Uint32'Read (Stream, Item.Body_Length);
      D_Message_Serial'Read (Stream, Item.Serial);
      Field_Map_Raw'Read (Stream, Item.Fields);

      --  Pad to 8 byte boundary
      D_Bus.Streams.Read_Align (Stream, 8);
   end Read_RMH;

   procedure Write_RMH
     (Stream : not null access Ada.Streams.Root_Stream_Type'Class;
      Item   : Raw_Message_Header)
   is
   begin
      Message_Endianness'Write (Stream, Item.Endianness);
      Message_Type'Write (Stream, Item.M_Type);
      Message_Flags'Write (Stream, Item.Flags);
      Interfaces.Unsigned_8'Write (Stream, Item.Protocol_Version);

      --  All further fields depend on endianness
      --  TODO endianness
      D_Bus.Types.Basic.Uint32'Write (Stream, Item.Body_Length);
      D_Message_Serial'Write (Stream, Item.Serial);
      Field_Map_Raw'Write (Stream, Item.Fields);

      --  Pad to 8 byte boundary
      D_Bus.Streams.Write_Align (Stream, 8);
   end Write_RMH;

   procedure Read
     (Stream :     not null access Ada.Streams.Root_Stream_Type'Class;
      Item   : out Message)
   is
      use D_Bus.Types.Containers;
      use type D_Bus.Types.Basic.Uint32;
      use type D_Bus.Types.Basic.D_Signature;
      use type D_Bus.Types.Basic.Byte;

      use type D_Message_Serial;

      use type Interfaces.Unsigned_8;
      use type Interfaces.Unsigned_32;

      function Field_From_Byte is new Ada.Unchecked_Conversion
        (Interfaces.Unsigned_8, Field_Type);

      RMH : Raw_Message_Header;
   begin
      --  Read raw header
      --  Note: Alignment reset before this!
      Raw_Message_Header'Read (Stream, RMH);

      --  TODO handle endianness here

      Item := (Serial => Valid_Message_Serial'(+RMH.Serial), others => <>);
      Item.M_Type := RMH.M_Type;
      Item.Flags  := RMH.Flags;

      --  When we read the fields back we can only get bytes :(
      --  There is no way to automatically determine that the actual type
      --  is Field_Type
      for Cursor in RMH.Fields.Iterate loop
         declare
            S : Struct renames Struct (Element (Cursor));
         begin
            Item.Fields.Insert
              (Field_From_Byte (+D_Bus.Types.Basic.Byte (S.Get (1))),
               Variant (S.Get (2)));
         end;
      end loop;

      --  Check that the required fields are contained for each type
      case RMH.M_Type is
         when Invalid =>
            raise Protocol_Error;

         when Method_Call =>
            Assert_Or_Protocol_Error (Item.Fields.Contains (F_Path));
            Assert_Or_Protocol_Error (Item.Fields.Contains (F_Member));

         when Method_Return =>
            Assert_Or_Protocol_Error (Item.Fields.Contains (F_Reply_Serial));

         when Error =>
            Assert_Or_Protocol_Error (Item.Fields.Contains (F_Reply_Serial));
            Assert_Or_Protocol_Error (Item.Fields.Contains (F_Error_Name));

         when Signal =>
            Assert_Or_Protocol_Error (Item.Fields.Contains (F_Path));
            Assert_Or_Protocol_Error (Item.Fields.Contains (F_Interface));
            Assert_Or_Protocol_Error (Item.Fields.Contains (F_Member));
      end case;

      --  Handle messages with no body
      if +RMH.Body_Length = 0 then
         return;
      end if;

      --  Read elements one by one
      declare
         Types : constant D_Bus.Types.Single_Signature_Array :=
           D_Bus.Types.Split_Signature
             (+D_Bus.Types.Basic.D_Signature (Item.Fields (F_Signature).Get));
      begin
         for Signature of Types loop
            Item.Arguments.Append
              (D_Bus.Types.Dispatching_Read (Stream, Signature.all));
         end loop;
      end;
   end Read;

   procedure Write
     (Stream :        not null access Ada.Streams.Root_Stream_Type'Class;
      Item   : in out Message)
   is
      use type D_Message_Serial;
      use type D_Field_Type;

      use type D_Bus.Types.Basic.D_Signature;
      use type D_Bus.Types.Basic.Uint32;

      RMH : Raw_Message_Header;
      S   : Valid_Message_Serial;
   begin
      --  Make serial
      Global_Serials.Next_Serial (S);
      Item :=
        (Serial => S, M_Type => Item.M_Type, Flags => Item.Flags,
         Fields => Item.Fields, Arguments => Item.Arguments);

      --  Prepare header
      RMH.M_Type      := Item.M_Type;
      RMH.Flags       := Item.Flags;
      RMH.Body_Length :=
        +Interfaces.Unsigned_32 (D_Bus.Types.Size (Item.Arguments));
      RMH.Serial      := +Item.Serial;

      --  Add all standard fields
      for Cursor in Item.Fields.Iterate loop
         declare
            use D_Bus.Types.Containers;

            S : Struct := Empty (Field_Struct_Contents);
         begin
            S.Set (1, +Field_Maps.Key (Cursor));
            S.Set (2, Field_Maps.Element (Cursor));

            RMH.Fields.Append (S);
         end;
      end loop;

      --  Add calculated fields (currently SIGNATURE)
      --  Note only if there are arguments
      if not Item.Arguments.Is_Empty then
         declare
            use D_Bus.Types.Containers;

            S : Struct := Empty (Field_Struct_Contents);
         begin
            S.Set (1, +F_Signature);
            S.Set (2, +(+D_Bus.Types.Signature (Item.Arguments)));

            RMH.Fields.Append (S);
         end;
      end if;

      --  Write header
      --  Note: Alignment reset before this :)
      Raw_Message_Header'Write (Stream, RMH);

      --  Write all elements
      for Element of Item.Arguments loop
         D_Bus.Types.Root_Type'Class'Write (Stream, Element);
      end loop;
   end Write;
end D_Bus.Messages;
