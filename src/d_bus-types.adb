pragma Ada_2012;

package body D_Bus.Types is


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
end D_Bus.Types;
