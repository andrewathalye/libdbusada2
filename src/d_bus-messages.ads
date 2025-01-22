pragma Ada_2012;

with D_Bus.Types.Containers;
with D_Bus.Types.Basic;

with Ada.Streams;
private with Ada.Containers.Ordered_Maps;

package D_Bus.Messages is
   pragma Assertion_Policy (Dynamic_Predicate => Check);
   pragma Assertion_Policy (Pre => Check);

   ------------------
   -- Base Message --
   ------------------
   type Message is private;
   --  A type representing a D-Bus message, either received
   --  over a `Connection` or not yet sent by the program.
   --
   --  It is permissible to resend a message that has already
   --  been sent: this new message will receive a different
   --  serial number. Bear in mind, however, that the client
   --  receiving this duplicate message may not be expecting
   --  it.

   -------------------
   -- Message Types --
   -------------------
   type Message_Type is (Invalid, Method_Call, Method_Return, Error, Signal);
   for Message_Type'Size use 8;
   for Message_Type use
     (Invalid => 0, Method_Call => 1, Method_Return => 2, Error => 3,
      Signal  => 4);

   -------------------
   -- Message Flags --
   -------------------
   type Message_Flags is record
      No_Reply_Expected                : Boolean := False;
      No_Auto_Start                    : Boolean := False;
      Allow_Interactive_Authentication : Boolean := False;
   end record;

   for Message_Flags use record
      No_Reply_Expected                at 0 range 0 .. 0;
      No_Auto_Start                    at 0 range 1 .. 1;
      Allow_Interactive_Authentication at 0 range 2 .. 2;
   end record;
   for Message_Flags'Size use 8;

   procedure Read
     (Stream :     not null access Ada.Streams.Root_Stream_Type'Class;
      Item   : out Message_Flags);
   procedure Write
     (Stream : not null access Ada.Streams.Root_Stream_Type'Class;
      Item   : Message_Flags);

   for Message_Flags'Read use Read;
   for Message_Flags'Write use Write;

   Default_Message_Flags : constant Message_Flags;

   -----------------------
   -- Validity Checking --
   -----------------------
   function Valid_Interface (X : String) return Boolean;
   function Valid_Member (X : String) return Boolean;
   function Valid_Bus (X : String) return Boolean;

   subtype Interface_Name is String with
       Dynamic_Predicate => Valid_Interface (Interface_Name),
       Predicate_Failure => "Invalid interface name " & Interface_Name;

   subtype Error_Name is Interface_Name;
   --  TODO check whether regex inherits

   subtype Member_Name is String with
       Dynamic_Predicate => Valid_Member (Member_Name),
       Predicate_Failure => "Invalid member name " & Member_Name;

   subtype Bus_Name is String with
       Dynamic_Predicate => Valid_Bus (Bus_Name),
       Predicate_Failure => "Invalid bus name " & Bus_Name;

   -------------------------
   -- Message Composition --
   -------------------------
   No_Reply_Expected : exception;

   function Compose_Call
     (Flags  : Message_Flags := Default_Message_Flags;
      Path : D_Bus.Types.Basic.Object_Path; M_Interface : Interface_Name := "";
      Member : Member_Name; Destination : Bus_Name := "") return Message;
   --  Prepare a message call

   function Compose_Return
     (Flags       : Message_Flags := Default_Message_Flags; Reply_To : Message;
      Destination : Bus_Name      := "") return Message;
   --  Prepare a message return
   --
   --  Raises `No_Reply_Expected` if `Reply_To` does
   --  not permit a reply.

   function Compose_Error
     (Flags    : Message_Flags := Default_Message_Flags; Error : Error_Name;
      Reply_To : Message; Destination : Bus_Name := "") return Message;
   --  Prepare an error message

   function Compose_Signal
     (Flags  : Message_Flags := Default_Message_Flags;
      Path   : D_Bus.Types.Basic.Object_Path; M_Interface : Interface_Name;
      Member : Member_Name) return Message;
   --  Prepare a signal

   procedure Add_Arguments
     (M : out Message; Arguments : D_Bus.Types.Argument_List);
   --  Add arguments to a message

   ---------------------
   -- Message Parsing --
   ---------------------
   function M_Type (M : Message) return Message_Type with
     Global => null;
   pragma Pure_Function (M_Type);

   Field_Absent : exception;
   --  Raised when the requested field is not present

   function Path (M : Message) return D_Bus.Types.Basic.Object_Path;
   --  Obligatory in Method_Call and Signal
   --  May raise `Field_Absent`

   function M_Interface (M : Message) return Interface_Name;
   --  Obligatory in Signal
   --  May raise `Field_Absent`

   function Member (M : Message) return Member_Name;
   --  Obligatory in Method_Call and Signal
   --  May raise `Field_Absent`

   function Error (M : Message) return Error_Name with
     Pre => M_Type (M) = Error;

   function Is_Reply (Original, Reply : Message) return Boolean with
     Pre =>
      M_Type (Original) = Method_Call and
      M_Type (Reply) in Method_Return | Error;

   function Destination (M : Message) return Bus_Name;
   function Sender (M : Message) return Bus_Name;
   function Signature (M : Message) return D_Bus.Types.Contents_Signature;
   --  Optional: may raise `Field_Absent`

   function Arguments (M : Message) return D_Bus.Types.Argument_List;
private
   Default_Message_Flags : constant Message_Flags := (others => <>);
   --  Message_Flags with everything set to default

   --------------------
   -- Message Fields --
   --------------------
   type Field_Type is
     (F_Invalid, F_Path, F_Interface, F_Member, F_Error_Name, F_Reply_Serial,
      F_Destination, F_Sender, F_Signature, F_Unix_Fds);
   for Field_Type'Size use 8;
   for Field_Type use
     (F_Invalid    => 0, F_Path => 1, F_Interface => 2, F_Member => 3,
      F_Error_Name => 4, F_Reply_Serial => 5, F_Destination => 6,
      F_Sender     => 7, F_Signature => 8, F_Unix_Fds => 9);

   use type D_Bus.Types.Containers.Variant;
   package Field_Maps is new Ada.Containers.Ordered_Maps
     (Key_Type => Field_Type, Element_Type => D_Bus.Types.Containers.Variant);

   -----------------------
   -- Message Internals --
   -----------------------
   Default_Protocol_Version : constant := 1;

   type U_Message_Serial is mod 2**32;
   for U_Message_Serial'Size use 32;
   Invalid_Serial : constant U_Message_Serial := 0;
   subtype Valid_Message_Serial is
     U_Message_Serial range 1 .. U_Message_Serial'Last;

   type Message (Serial : U_Message_Serial := Invalid_Serial) is record
      M_Type    : Message_Type;
      Flags     : Message_Flags;
      Fields    : Field_Maps.Map;
      Arguments : D_Bus.Types.Argument_List;
   end record;
   --  Note: this is not the internal representation over the wire
   procedure Read
     (Stream :     not null access Ada.Streams.Root_Stream_Type'Class;
      Item   : out Message);
   procedure Write
     (Stream : not null access Ada.Streams.Root_Stream_Type'Class;
      Item   : Message);

   for Message'Read use Read;
   for Message'Write use Write;
end D_Bus.Messages;
