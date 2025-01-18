pragma Ada_2012;

with Ada.Unchecked_Conversion;
with D_Bus.Types.Dispatching_Read;
with Interfaces;
with System;

with D_Bus.Types.Basic_Generic;
with GNAT.Regexp;

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
         Current_Serial := Current_Serial + 1;
         S              := Current_Serial;
      end Next_Serial;
   end Global_Serials;

   package D_Message_Serials is new D_Bus.Types.Basic_Generic.Fixed_Wrappers
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

   -----------------------
   -- Validity Checking --
   -----------------------
   Interface_Regexp : constant GNAT.Regexp.Regexp :=
     GNAT.Regexp.Compile
       (Pattern => "^([a-zA-Z_]([a-zA-Z0-9_])*\.)+[a-zA-Z_]([a-zA-Z0-9_])*",
        Glob    => True, Case_Sensitive => True);
   function Valid_Interface (X : String) return Boolean is
   begin
      return GNAT.Regexp.Match (X, Interface_Regexp) and X'Length < 256;
   end Valid_Interface;

   ------------------
   -- Valid_Member --
   ------------------
   Member_Regexp : constant GNAT.Regexp.Regexp :=
     GNAT.Regexp.Compile
       (Pattern => "^([a-zA-Z0-9_])*$", Glob => True, Case_Sensitive => True);
   function Valid_Member (X : String) return Boolean is
   begin
      return GNAT.Regexp.Match (X, Member_Regexp) and X'Length < 256;
   end Valid_Member;

   ---------------
   -- Valid_Bus --
   ---------------
   Bus_Regexp : constant GNAT.Regexp.Regexp :=
     GNAT.Regexp.Compile
       (Pattern =>
          "^((:[A-Za-z0-9])|([A-Za-z]))(\.[a-zA-Z0-9]([a-zA-Z0-9_-])*)*$",
        Glob    => True, Case_Sensitive => True);
   function Valid_Bus (X : String) return Boolean is
   begin
      return GNAT.Regexp.Match (X, Bus_Regexp) and X'Length < 256;
   end Valid_Bus;

   ------------------
   -- Compose_Call --
   ------------------
   function Compose_Call
     (Flags  : Message_Flags := Default_Message_Flags;
      Path : D_Bus.Types.Basic.Object_Path; M_Interface : Interface_Name := "";
      Member : Member_Name; Destination : Bus_Name := "") return Message
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
      Destination : Bus_Name      := "") return Message
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
     (Flags    : Message_Flags := Default_Message_Flags; Error : Error_Name;
      Reply_To : Message; Destination : Bus_Name := "") return Message
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
     (Flags  : Message_Flags := Default_Message_Flags;
      Path   : D_Bus.Types.Basic.Object_Path; M_Interface : Interface_Name;
      Member : Member_Name) return Message
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
     (M : out Message; Arguments : in out D_Bus.Types.Argument_List)
   is
   begin
      M.Arguments.Splice
        (Before => D_Bus.Types.Argument_Lists.No_Element, Source => Arguments);
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
   function Path (M : Message) return D_Bus.Types.Basic.Object_Path is
      use type D_Bus.Types.Basic.D_Object_Path;
   begin
      return +D_Bus.Types.Basic.D_Object_Path (M.Fields (F_Path).Get);
   end Path;

   function M_Interface (M : Message) return Interface_Name is
      use type D_Bus.Types.Basic.D_String;
   begin
      return +D_Bus.Types.Basic.D_String (M.Fields (F_Interface).Get);
   end M_Interface;

   function Member (M : Message) return Member_Name is
      use type D_Bus.Types.Basic.D_String;
   begin
      return +D_Bus.Types.Basic.D_String (M.Fields (F_Member).Get);
   end Member;

   function Error (M : Message) return Error_Name is
      use type D_Bus.Types.Basic.D_String;
   begin
      return +D_Bus.Types.Basic.D_String (M.Fields (F_Error_Name).Get);
   end Error;

   function Is_Reply (Original, Reply : Message) return Boolean is
      use type D_Message_Serial;
   begin
      return
        Original.Flags.No_Reply_Expected
        and then Original.Serial =
          +D_Message_Serial (Reply.Fields (F_Reply_Serial).Get);
   end Is_Reply;

   function Destination (M : Message) return Bus_Name is
      use type D_Bus.Types.Basic.D_String;
   begin
      return +D_Bus.Types.Basic.D_String (M.Fields (F_Destination).Get);
   end Destination;

   function Sender (M : Message) return Bus_Name is
      use type D_Bus.Types.Basic.D_String;
   begin
      return +D_Bus.Types.Basic.D_String (M.Fields (F_Sender).Get);
   end Sender;

   function Signature (M : Message) return D_Bus.Types.Contents_Signature is
      use type D_Bus.Types.Basic.D_Signature;
   begin
      return +D_Bus.Types.Basic.D_Signature (M.Fields (F_Signature).Get);
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
   --  type ME_Table_To_Ada_T is array (Message_Endianness)
   --     of System.Bit_Order;
   ME_Table_From_Ada          : constant ME_Table_From_Ada_T := (Big, Little);
   --   ME_Table_To_Ada : constant ME_Table_To_Ada_T :=
   --     (System.High_Order_First, System.Low_Order_First);
   Default_Message_Endianness : constant Message_Endianness  :=
     ME_Table_From_Ada (System.Default_Bit_Order);
   --  TODO actually implement

   package D_Field_Types is new D_Bus.Types.Basic_Generic.Fixed_Wrappers
     (Type_Code => 'y', Inner => Field_Type);
   subtype D_Field_Type is D_Field_Types.Outer;

   type RMH_Padding_NR is null record;
   package RMH_Padding is new D_Bus.Types.Padded_Types (RMH_Padding_NR, 8);

   subtype Field_Dict is
     D_Bus.Types.Containers.Dict ('y', D_Bus.Types.Intern ("v"));

   type Raw_Message_Header is record
      Endianness       : Message_Endianness    := Default_Message_Endianness;
      M_Type           : Message_Type;
      Flags            : Message_Flags;
      Protocol_Version : Interfaces.Unsigned_8 := Default_Protocol_Version;
      Body_Length      : D_Bus.Types.Basic.Uint32;
      Serial           : D_Message_Serial;
      Fields           : Field_Dict;
      Padding          : RMH_Padding.Padded_Type;
   end record;

   procedure Read
     (Stream :     not null access Ada.Streams.Root_Stream_Type'Class;
      Item   : out Message)
   is
      use D_Bus.Types.Containers;
      use type D_Bus.Types.Basic.Uint32;
      use type D_Bus.Types.Basic.D_Signature;

      use type D_Message_Serial;
      use type D_Field_Type;

      use type Interfaces.Unsigned_8;
      use type Interfaces.Unsigned_32;

      RMH : Raw_Message_Header;
   begin
      --  Read raw header
      Raw_Message_Header'Read (Stream, RMH);

      if RMH.Endianness /= Default_Message_Endianness then
         raise Protocol_Error with "Only native endianness supported";
      end if;

      if RMH.Protocol_Version /= Default_Protocol_Version then
         raise Protocol_Error with "Wrong protocol version";
      end if;

      Item := (Serial => Valid_Message_Serial'(+RMH.Serial), others => <>);
      Item.M_Type := RMH.M_Type;
      Item.Flags  := RMH.Flags;

      for Cursor in RMH.Fields.Iterate_D loop
         Item.Fields.Insert
           (+D_Field_Type (Key (Cursor)), Variant (Element (Cursor)));
      end loop;

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
     (Stream : not null access Ada.Streams.Root_Stream_Type'Class;
      Item   : Message)
   is
      use type D_Message_Serial;
      use type D_Field_Type;

      use type D_Bus.Types.Basic.D_Signature;
      use type D_Bus.Types.Basic.Uint32;

      RMH : Raw_Message_Header;
      S   : Valid_Message_Serial;
   begin
      --  Prepare header
      RMH.M_Type      := Item.M_Type;
      RMH.Flags       := Item.Flags;
      RMH.Body_Length :=
        +Interfaces.Unsigned_32 (D_Bus.Types.Size (Item.Arguments));
      Global_Serials.Next_Serial (S);
      RMH.Serial := +S;

      --  Add all standard fields
      for Cursor in Item.Fields.Iterate loop
         RMH.Fields.Insert
           (+Field_Maps.Key (Cursor), Field_Maps.Element (Cursor));
      end loop;

      --  Add calculated fields (currently SIGNATURE)
      RMH.Fields.Insert
        (+F_Signature, +D_Bus.Types.Signature (Item.Arguments));

      --  Write header
      Raw_Message_Header'Write (Stream, RMH);

      --  Write all elements
      for Element of Item.Arguments loop
         D_Bus.Types.Root_Type'Class'Write (Stream, Element);
      end loop;
   end Write;
end D_Bus.Messages;
