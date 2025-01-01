pragma Ada_2012;

with GNAT.Regexp;

package body D_Bus.Types is

   -------------------------
   -- Regular Expressions --
   -------------------------
   --  From dbus-binding-generator-ada/share/introspect.xsd
   Object_Pattern : constant GNAT.Regexp.Regexp := GNAT.Regexp.Compile
     (Pattern => "^\/?(([a-zA-Z0-9_])+(\/([a-zA-Z0-9_])+)?)+$|^\/$",
      Glob => True,
      Case_Sensitive => True);

   Signature_Pattern : constant GNAT.Regexp.Regexp := GNAT.Regexp.Compile
     (Pattern => "^[ybnqiuxtdsogvh]$|^a([ybnqiuxtdsogavh\(\)\{\}])+$|^\(([y"
         & "bnqiuxtdsogavh\(\)])+\)$",
      Glob => True,
      Case_Sensitive => True);

   -----------
   -- Valid --
   -----------
   function Valid (X : Object_Paths.Outer) return Boolean is
      use Object_Paths;
   begin
      return GNAT.Regexp.Match (+X, Object_Pattern);
   end Valid;

   -----------
   -- Valid --
   -----------
   function Valid (X : Signatures.Outer) return Boolean is
      use Signatures;
   begin
      return Length (X) <= 255
         and then GNAT.Regexp.Match
           (+X, Signature_Pattern);
   end Valid;

   -----------
   -- Valid --
   -----------
   function Valid (X : U_Variant) return Boolean is
   begin
      return X.I /= null;
   end Valid;

   ---------
   -- "+" --
   ---------
   function "+" (X : Root_Type'Class) return U_Variant is
   begin
      return (Container_Type with I => new Root_Type'Class'(X));
   end "+";

   ---------
   -- "+" --
   ---------
   function "+" (X : U_Variant) return Root_Type'Class is
   begin
      return X.I.all;
   end "+";

   ----------------------
   -- Dispatching_Read --
   ----------------------
   function Dispatching_Read
     (Stream : not null access Ada.Streams.Root_Stream_Type'Class;
      Signature : String) return Root_Type'Class;

   function Dispatching_Read
     (Stream : not null access Ada.Streams.Root_Stream_Type'Class;
      Signature : String) return Root_Type'Class
   is
      FirstTwo : constant String := Signature
        (Signature'First ..
           (if Signature'Length = 1 then
               Signature'First
            else Signature'First + 1));
      First : constant Character := Signature (Signature'First);
   begin
      if FirstTwo = "a{" then
         --  TODO handle dict reading ::::(
         raise Program_Error;
      end if;

      case First is
         when 'y' =>
            declare
               T : Byte;
            begin
               Byte'Read (Stream, T);
               return T;
            end;
         when 'b' =>
            declare
               T : D_Boolean;
            begin
               D_Boolean'Read (Stream, T);
               return T;
            end;
         when 'n' =>
            declare
               T : Int16;
            begin
               Int16'Read (Stream, T);
               return T;
            end;
         when 'q' =>
            declare
               T : Uint16;
            begin
               Uint16'Read (Stream, T);
               return T;
            end;
         when 'i' =>
            declare
               T : Int32;
            begin
               Int32'Read (Stream, T);
               return T;
            end;
         when 'u' =>
            declare
               T : Uint32;
            begin
               Uint32'Read (Stream, T);
               return T;
            end;
         when 'x' =>
            declare
               T : Int64;
            begin
               Int64'Read (Stream, T);
               return T;
            end;
         when 't' =>
            declare
               T : Uint64;
            begin
               Uint64'Read (Stream, T);
               return T;
            end;
         when 'd' =>
            declare
               T : Double;
            begin
               Double'Read (Stream, T);
               return T;
            end;
         when 'h' =>
            --  TODO
            null;
         when 's' =>
            declare
               T : D_String;
            begin
               D_String'Read (Stream, T);
               return T;
            end;
         when 'o' =>
            declare
               T : Object_Path;
            begin
               Object_Path'Read (Stream, T);
               return T;
            end;
         when 'g' =>
            declare
               T : D_Signature;
            begin
               D_Signature'Read (Stream, T);
               return T;
            end;
         when '(' =>
            --  TODO
            null;
         when 'a' =>
            --  TODO
            null;
         when 'v' =>
            declare
               T : Variant;
            begin
               Variant'Read (Stream, T);
               return T;
            end;
         when others => raise Program_Error;
      end case;

      return raise Program_Error;
   end Dispatching_Read;

   ------------------
   -- Read_Variant --
   ------------------
   procedure Read_Variant
     (Stream :     not null access Ada.Streams.Root_Stream_Type'Class;
      Item   : out U_Variant)
   is
      use Signatures;
      Inner_Signature : D_Signature;
   begin
      D_Signature'Read (Stream, Inner_Signature);
      Item.I := new Root_Type'Class'(
         Dispatching_Read (Stream, +Inner_Signature));
   end Read_Variant;

   -------------------
   -- Write_Variant --
   -------------------
   procedure Write_Variant
     (Stream : not null access Ada.Streams.Root_Stream_Type'Class;
      Item   : U_Variant)
   is
      use Signatures;
      Inner_Signature : constant D_Signature := +Item.Signature;
   begin
      D_Signature'Write (Stream, Inner_Signature);
      Root_Type'Class'Write (Stream, Item.I.all);
   end Write_Variant;

end D_Bus.Types;
