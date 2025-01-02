pragma Ada_2012;

with D_Bus.Types;
   use type D_Bus.Types.Uint32;
with Interfaces;
   use type Interfaces.Unsigned_32;

private with Ada.Streams;
private with Ada.Containers.Doubly_Linked_Lists;

package D_Bus.Messages is
   pragma Assertion_Policy (Dynamic_Predicate => Check);

   ------------------
   -- Base Message --
   ------------------
   type U_Message is private;
   --  Call `Compose_X` to produce a valid message.
   --  Call `Fill` to add arguments to a message

   subtype Message_Serial is D_Bus.Types.Uint32
   with Dynamic_Predicate => +Message_Serial > 0;

   type Message_Flags is record
      No_Reply_Expected : Boolean;
      No_Auto_Start : Boolean;
      Allow_Interactive_Authentication : Boolean;
   end record;

   for Message_Flags use
     record
        No_Reply_Expected at 0 range 0 .. 0;
        No_Auto_Start at 0 range 1 .. 1;
        Allow_Interactive_Authentication at 0 range 2 .. 2;
     end record;
   for Message_Flags'Size use 8;

   -----------------------
   -- Validity Checking --
   -----------------------
   function Valid_Interface (X : String) return Boolean;
   function Valid_Member (X : String) return Boolean;
   function Valid_Bus (X : String) return Boolean;

   subtype Interface_Name is String
   with Dynamic_Predicate => Valid_Interface (Interface_Name);

   subtype Error_Name is Interface_Name;
   --  TODO check whether regex applies

   subtype Member_Name is String
   with Dynamic_Predicate => Valid_Member (Member_Name);

   subtype Bus_Name is String
   with Dynamic_Predicate => Valid_Bus (Bus_Name);

   function Valid (M : U_Message) return Boolean;
   --  Check whether a message is valid and may
   --  be sent over a connection.

   subtype Message is U_Message
   with Dynamic_Predicate => Valid (Message);

   ------------------------
   -- Message Allocation --
   ------------------------
   function Compose_Call
     (Flags : Message_Flags;
      Path  : D_Bus.Types.Object_Path;
      M_Interface : Interface_Name := "";
      Member : Member_Name;
      Destination : Bus_Name := "") return Message;
   --  Prepare a message call

   function Compose_Return
     (Flags : Message_Flags;
      Reply_Serial : Message_Serial;
      Destination : Bus_Name := "") return Message;
   --  Prepare a message return

   function Compose_Error
      (Flags : Message_Flags;
       Reply_Serial : Message_Serial;
       Destination : Bus_Name := "") return Message;
   --  Prepare an error message

   function Compose_Signal
      (Flags : Message_Flags;
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
   Field_Absent : exception;
   --  Raised when the requested field is not available

   function Object_Path (M : Message) return D_Bus.Types.Object_Path;
   function M_Interface (M : Message) return Interface_Name;
   function Member (M : Message) return Member_Name;
   function Error (M : Message) return Error_Name;
   function Reply_Serial (M : Message) return Message_Serial;
   function Destination (M : Message) return Bus_Name;
   function Sender (M : Message) return Bus_Name;
   function Signature (M : Message) return D_Bus.Types.D_Signature;
private
   --  Message types
   type Message_Type is
     (Invalid, Method_Call, Method_Return, Error, Signal);
   for Message_Type'Size use 8;
   for Message_Type use
     (Invalid => 0,
      Method_Call => 1,
      Method_Return => 2,
      Error => 3,
      Signal => 4);

   --  Message endianness
   type Message_Endianness is (Big, Little);
   for Message_Endianness'Size use 8;
   for Message_Endianness use
     (Big => Character'Pos ('B'),
      Little => Character'Pos ('l'));

   --  Variant field implementation
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

   package Field_Lists is new Ada.Containers.Doubly_Linked_Lists (Field);
   subtype Field_List is Field_Lists.List;

   --  Message internals
   type U_Message (Valid : Boolean := False) is record
      Endianness : Message_Endianness;
      M_Type : Message_Type;
      Flags : Message_Flags;
      Protocol_Version : D_Bus.Types.Byte := D_Bus.Types.Bytes."+" (1);
      Body_Length : D_Bus.Types.Uint32;
      Serial : Message_Serial;
      Fields : Field_List;
      M_Body : D_Bus.Types.Argument_List;
   end record;

   procedure Read
     (Stream : not null access Ada.Streams.Root_Stream_Type'Class;
      Item : out U_Message);
   procedure Write
     (Stream : not null access Ada.Streams.Root_Stream_Type'Class;
      Item : U_Message);

   for U_Message'Read use Read;
   for U_Message'Write use Write;
   --  TODO handle endianness
end D_Bus.Messages;
