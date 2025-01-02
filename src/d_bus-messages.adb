pragma Ada_2012;
package body D_Bus.Messages is

   ---------------------
   -- Valid_Interface --
   ---------------------

   function Valid_Interface (X : String) return Boolean is
   begin
      pragma Compile_Time_Warning
        (Standard.True, "Valid_Interface unimplemented");
      return raise Program_Error with "Unimplemented function Valid_Interface";
   end Valid_Interface;

   ------------------
   -- Valid_Member --
   ------------------

   function Valid_Member (X : String) return Boolean is
   begin
      pragma Compile_Time_Warning
        (Standard.True, "Valid_Member unimplemented");
      return raise Program_Error with "Unimplemented function Valid_Member";
   end Valid_Member;

   ---------------
   -- Valid_Bus --
   ---------------

   function Valid_Bus (X : String) return Boolean is
   begin
      pragma Compile_Time_Warning (Standard.True, "Valid_Bus unimplemented");
      return raise Program_Error with "Unimplemented function Valid_Bus";
   end Valid_Bus;

   -----------
   -- Valid --
   -----------

   function Valid (M : U_Message) return Boolean is
   begin
      pragma Compile_Time_Warning (Standard.True, "Valid unimplemented");
      return raise Program_Error with "Unimplemented function Valid";
   end Valid;

   ------------------
   -- Compose_Call --
   ------------------

   function Compose_Call
     (Flags       : Message_Flags; Path : D_Bus.Types.Object_Path;
      M_Interface : Interface_Name := ""; Member : Member_Name;
      Destination : Bus_Name       := "") return Message
   is
   begin
      pragma Compile_Time_Warning
        (Standard.True, "Compose_Call unimplemented");
      return raise Program_Error with "Unimplemented function Compose_Call";
   end Compose_Call;

   --------------------
   -- Compose_Return --
   --------------------

   function Compose_Return
     (Flags       : Message_Flags; Reply_Serial : Message_Serial;
      Destination : Bus_Name := "") return Message
   is
   begin
      pragma Compile_Time_Warning
        (Standard.True, "Compose_Return unimplemented");
      return raise Program_Error with "Unimplemented function Compose_Return";
   end Compose_Return;

   -------------------
   -- Compose_Error --
   -------------------

   function Compose_Error
     (Flags       : Message_Flags; Reply_Serial : Message_Serial;
      Destination : Bus_Name := "") return Message
   is
   begin
      pragma Compile_Time_Warning
        (Standard.True, "Compose_Error unimplemented");
      return raise Program_Error with "Unimplemented function Compose_Error";
   end Compose_Error;

   --------------------
   -- Compose_Signal --
   --------------------

   function Compose_Signal
     (Flags       : Message_Flags; Path : D_Bus.Types.Object_Path;
      M_Interface : Interface_Name; Member : Member_Name) return Message
   is
   begin
      pragma Compile_Time_Warning
        (Standard.True, "Compose_Signal unimplemented");
      return raise Program_Error with "Unimplemented function Compose_Signal";
   end Compose_Signal;

   -------------------
   -- Add_Arguments --
   -------------------

   procedure Add_Arguments
     (M : out Message; Arguments : D_Bus.Types.Argument_List)
   is
   begin
      pragma Compile_Time_Warning
        (Standard.True, "Add_Arguments unimplemented");
      raise Program_Error with "Unimplemented procedure Add_Arguments";
   end Add_Arguments;

   -----------------
   -- Object_Path --
   -----------------

   function Object_Path (M : Message) return D_Bus.Types.Object_Path is
   begin
      pragma Compile_Time_Warning (Standard.True, "Object_Path unimplemented");
      return raise Program_Error with "Unimplemented function Object_Path";
   end Object_Path;

   -----------------
   -- M_Interface --
   -----------------

   function M_Interface (M : Message) return Interface_Name is
   begin
      pragma Compile_Time_Warning (Standard.True, "M_Interface unimplemented");
      return raise Program_Error with "Unimplemented function M_Interface";
   end M_Interface;

   ------------
   -- Member --
   ------------

   function Member (M : Message) return Member_Name is
   begin
      pragma Compile_Time_Warning (Standard.True, "Member unimplemented");
      return raise Program_Error with "Unimplemented function Member";
   end Member;

   -----------
   -- Error --
   -----------

   function Error (M : Message) return Error_Name is
   begin
      pragma Compile_Time_Warning (Standard.True, "Error unimplemented");
      return raise Program_Error with "Unimplemented function Error";
   end Error;

   ------------------
   -- Reply_Serial --
   ------------------

   function Reply_Serial (M : Message) return Message_Serial is
   begin
      pragma Compile_Time_Warning
        (Standard.True, "Reply_Serial unimplemented");
      return raise Program_Error with "Unimplemented function Reply_Serial";
   end Reply_Serial;

   -----------------
   -- Destination --
   -----------------

   function Destination (M : Message) return Bus_Name is
   begin
      pragma Compile_Time_Warning (Standard.True, "Destination unimplemented");
      return raise Program_Error with "Unimplemented function Destination";
   end Destination;

   ------------
   -- Sender --
   ------------

   function Sender (M : Message) return Bus_Name is
   begin
      pragma Compile_Time_Warning (Standard.True, "Sender unimplemented");
      return raise Program_Error with "Unimplemented function Sender";
   end Sender;

   ---------------
   -- Signature --
   ---------------

   function Signature (M : Message) return D_Bus.Types.D_Signature is
   begin
      pragma Compile_Time_Warning (Standard.True, "Signature unimplemented");
      return raise Program_Error with "Unimplemented function Signature";
   end Signature;

   ----------
   -- Read --
   ----------

   procedure Read
     (Stream :     not null access Ada.Streams.Root_Stream_Type'Class;
      Item   : out U_Message)
   is
   begin
      pragma Compile_Time_Warning (Standard.True, "Read unimplemented");
      raise Program_Error with "Unimplemented procedure Read";
   end Read;

   -----------
   -- Write --
   -----------

   procedure Write
     (Stream : not null access Ada.Streams.Root_Stream_Type'Class;
      Item   : U_Message)
   is
   begin
      pragma Compile_Time_Warning (Standard.True, "Write unimplemented");
      raise Program_Error with "Unimplemented procedure Write";
   end Write;

end D_Bus.Messages;
