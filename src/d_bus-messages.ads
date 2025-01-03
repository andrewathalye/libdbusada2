pragma Ada_2012;

with D_Bus.Types;
   use type D_Bus.Types.Uint32;
   use type D_Bus.Types.D_String;
with Interfaces;
   use type Interfaces.Unsigned_32;

with Ada.Streams;
private with Ada.Containers.Ordered_Maps;
private with System;

package D_Bus.Messages is
   pragma Assertion_Policy (Dynamic_Predicate => Check);

   ------------------
   -- Base Message --
   ------------------
   type U_Message is limited private;
   --  Underlying message type. See `Message` for documentation.
   --  TODO limited?
   --  Note message serial should not be reused...

   -------------------
   -- Message Types --
   -------------------
   type Message_Type is
     (Invalid, Method_Call, Method_Return, Error, Signal);
   for Message_Type'Size use 8;
   for Message_Type use
     (Invalid => 0,
      Method_Call => 1,
      Method_Return => 2,
      Error => 3,
      Signal => 4);

   -------------------
   -- Message Flags --
   -------------------
   type Message_Flags is record
      No_Reply_Expected : Boolean := False;
      No_Auto_Start : Boolean := False;
      Allow_Interactive_Authentication : Boolean := False;
   end record;

   for Message_Flags use
     record
        No_Reply_Expected at 0 range 0 .. 0;
        No_Auto_Start at 0 range 1 .. 1;
        Allow_Interactive_Authentication at 0 range 2 .. 2;
     end record;
   for Message_Flags'Size use 8;

   procedure Read
     (Stream : not null access Ada.Streams.Root_Stream_Type'Class;
      Item : out Message_Flags);
   procedure Write
     (Stream : not null access Ada.Streams.Root_Stream_Type'Class;
      Item : Message_Flags);

   for Message_Flags'Read use Read;
   for Message_Flags'Write use Write;

   Default_Message_Flags : constant Message_Flags;

   -----------------------
   -- Validity Checking --
   -----------------------
   function Valid_Interface (X : String) return Boolean;
   function Valid_Member (X : String) return Boolean;
   function Valid_Bus (X : String) return Boolean;

   subtype Interface_Name is D_Bus.Types.D_String
   with Dynamic_Predicate => Valid_Interface (+Interface_Name);

   subtype Error_Name is Interface_Name;
   --  TODO check whether regex inherits

   subtype Member_Name is D_Bus.Types.D_String
   with Dynamic_Predicate => Valid_Member (+Member_Name);

   subtype Bus_Name is D_Bus.Types.D_String
   with Dynamic_Predicate => Valid_Bus (+Bus_Name);

   function Valid_Message (M : U_Message) return Boolean;
   --  Check whether a message is valid and may
   --  be sent over a connection.

   subtype Message is U_Message
   with Dynamic_Predicate => Valid_Message (Message);
   --  A Valid D-Bus Message. Produce one as follows:
   --  Compose_X -> Add_Arguments -> [Send over the wire]

   -------------------------
   -- Message Composition --
   -------------------------
   No_Reply_Expected : exception;

   function Compose_Call
     (Flags : Message_Flags := Default_Message_Flags;
      Path  : D_Bus.Types.Object_Path;
      M_Interface : Interface_Name := +"";
      Member : Member_Name;
      Destination : Bus_Name := +"") return Message;
   --  Prepare a message call

   function Compose_Return
     (Flags : Message_Flags := Default_Message_Flags;
      Reply_To : Message;
      Destination : Bus_Name := +"") return Message;
   --  Prepare a message return
   --
   --  Raises `No_Reply_Expected` if `Reply_To` does
   --  not permit a reply.

   function Compose_Error
      (Flags : Message_Flags := Default_Message_Flags;
       Error : Error_Name;
       Reply_To : Message;
       Destination : Bus_Name := +"") return Message;
   --  Prepare an error message

   function Compose_Signal
      (Flags : Message_Flags := Default_Message_Flags;
       Path : D_Bus.Types.Object_Path;
       M_Interface : Interface_Name;
       Member : Member_Name) return Message;
   --  Prepare a signal

   procedure Add_Arguments
      (M : out Message;
       Arguments : D_Bus.Types.Argument_List);
   --  Add arguments to a message

   ---------------------
   -- Message Parsing --
   ---------------------
   function M_Type (M : Message) return Message_Type;

   Field_Absent : exception;
   --  Raised when the requested field is not present
   --  TODO currently we’ll get constraint error instead

   function Path (M : Message) return D_Bus.Types.Object_Path;
   --  Obligatory in Method_Call and Signal
   --  May raise `Field_Absent`

   function M_Interface (M : Message) return Interface_Name;
   --  Obligatory in Signal
   --  May raise `Field_Absent`

   function Member (M : Message) return Member_Name;
   --  Obligatory in Method_Call and Signal
   --  May raise `Field_Absent`

   function Error (M : Message) return Error_Name;
   --  Present only in Error
   --  May raise `Field_Absent`

   function Is_Reply (Original, Reply : Message) return Boolean;
   --  Present only in Method_Return and Error
   --  May raise `Field_Absent`

   function Destination (M : Message) return Bus_Name;
   function Sender (M : Message) return Bus_Name;
   function Signature (M : Message) return D_Bus.Types.D_Signature;
   --  Optional: may raise `Field_Absent`

   function Arguments (M : Message) return D_Bus.Types.Argument_List;
private
   use type D_Bus.Types.Byte;
   use type D_Bus.Types.Variant;

   Default_Message_Flags : constant Message_Flags := (others => <>);
   --  Message_Flags with everything set to default

   -------------------------
   --  Message Endianness --
   -------------------------
   type Message_Endianness is (Big, Little);
   for Message_Endianness'Size use 8;
   for Message_Endianness use
     (Big => Character'Pos ('B'),
      Little => Character'Pos ('l'));

   type ME_Table_From_Ada_T is array (System.Bit_Order) of Message_Endianness;
   type ME_Table_To_Ada_T is array (Message_Endianness) of System.Bit_Order;
   ME_Table_From_Ada : constant ME_Table_From_Ada_T := (Big, Little);
   ME_Table_To_Ada : constant ME_Table_To_Ada_T :=
     (System.High_Order_First, System.Low_Order_First);
   --  TODO actually implement

   --------------------
   -- Message Fields --
   --------------------
   type Field_Type is (F_Invalid, F_Path, F_Interface, F_Member, F_Error_Name,
      F_Reply_Serial, F_Destination, F_Sender, F_Signature, F_Unix_Fds);
   for Field_Type'Size use 8;
   for Field_Type use
     (F_Invalid => 0,
      F_Path => 1,
      F_Interface => 2,
      F_Member => 3,
      F_Error_Name => 4,
      F_Reply_Serial => 5,
      F_Destination => 6,
      F_Sender => 7,
      F_Signature => 8,
      F_Unix_Fds => 9);

   type Field is record
      Id : Field_Type;
      Contents : D_Bus.Types.Variant;
   end record;

   package Field_Maps is new Ada.Containers.Ordered_Maps
     (Field_Type, D_Bus.Types.Variant);
   subtype Field_Map is Field_Maps.Map;

   -----------------------
   -- Message Internals --
   -----------------------
   Default_Protocol_Version : constant D_Bus.Types.Byte := +1;

   subtype Message_Serial is D_Bus.Types.Uint32
   with Dynamic_Predicate => +Message_Serial > 0;

   --  Note: Compiler optimisation causes the case not to match
   pragma Style_Checks (Off, Little);
   --  this is not important, just suppress the warning

   type U_Message (Valid : Boolean := False) is record
      --  Unpadded, do not depend on byte order
      Endianness : Message_Endianness :=
        ME_Table_From_Ada (System.Default_Bit_Order);
      M_Type : Message_Type;
      Flags : Message_Flags;
      Protocol_Version : D_Bus.Types.Byte := Default_Protocol_Version;

      --  Padded, depend on byte order
      Body_Length : D_Bus.Types.Uint32 := +0;
      Serial : Message_Serial;
      Fields : Field_Map := Field_Maps.Empty_Map;

      --  Body, not header proper
      Arguments : D_Bus.Types.Argument_List :=
         D_Bus.Types.Argument_Lists.Empty_List;
   end record;

   procedure Read
     (Stream : not null access Ada.Streams.Root_Stream_Type'Class;
      Item : out U_Message);
   procedure Write
     (Stream : not null access Ada.Streams.Root_Stream_Type'Class;
      Item : U_Message);

   for U_Message'Read use Read;
   for U_Message'Write use Write;
end D_Bus.Messages;
