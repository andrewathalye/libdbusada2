pragma Ada_2012;

with D_Bus.Types.Basic;      use D_Bus.Types.Basic;
with D_Bus.Types.Containers; use D_Bus.Types.Containers;

function D_Bus.Types.Dispatching_Read
  (Stream    : not null access Ada.Streams.Root_Stream_Type'Class;
   Signature : Single_Signature) return Root_Type'Class
is
begin
   if Signature'Length = 0 then
      raise Constraint_Error;
   end if;

   case Signature (Signature'First) is
      --  Basic Types
      when Byte_CC =>
         declare
            Temp : Byte;
         begin
            Byte'Read (Stream, Temp);
            return Temp;
         end;
      when Boolean_CC =>
         declare
            Temp : D_Boolean;
         begin
            D_Boolean'Read (Stream, Temp);
            return Temp;
         end;
      when Int16_CC =>
         declare
            Temp : Int16;
         begin
            Int16'Read (Stream, Temp);
            return Temp;
         end;
      when Uint16_CC =>
         declare
            Temp : Uint16;
         begin
            Uint16'Read (Stream, Temp);
            return Temp;
         end;
      when Int32_CC =>
         declare
            Temp : Int32;
         begin
            Int32'Read (Stream, Temp);
            return Temp;
         end;
      when Uint32_CC =>
         declare
            Temp : Uint32;
         begin
            Uint32'Read (Stream, Temp);
            return Temp;
         end;
      when Int64_CC =>
         declare
            Temp : Int64;
         begin
            Int64'Read (Stream, Temp);
            return Temp;
         end;
      when Uint64_CC =>
         declare
            Temp : Uint64;
         begin
            Uint64'Read (Stream, Temp);
            return Temp;
         end;
      when Double_CC =>
         declare
            Temp : Double;
         begin
            Double'Read (Stream, Temp);
            return Temp;
         end;
      when File_Descriptor_CC =>
         declare
            Temp : File_Descriptor;
         begin
            File_Descriptor'Read (Stream, Temp);
            return Temp;
         end;
      when String_CC =>
         declare
            Temp : D_String;
         begin
            D_String'Read (Stream, Temp);
            return Temp;
         end;
      when Object_Path_CC =>
         declare
            Temp : D_Object_Path;
         begin
            D_Object_Path'Read (Stream, Temp);
            return Temp;
         end;
      when Signature_CC =>
         declare
            Temp : D_Signature;
         begin
            D_Signature'Read (Stream, Temp);
            return Temp;
         end;
         --  Container Types
      when Variant_CC =>
         return Temp : Variant do
            Variant'Read (Stream, Temp);
         end return;
      when Struct_Start_CC =>
         return
           Temp : Struct :=
             Empty
               (Contents_Signature
                  (Signature (Signature'First + 1 .. Signature'Last - 1)))
         do
            Struct'Read (Stream, Temp);
         end return;
      when Array_CC =>
         case Signature (Signature'First + 1) is
            when Dict_Start_CC =>
               return
                 Temp : Dict
                   (Key_Signature     =>
                      Signature (Signature'First + 2 .. Signature'First + 2)
                        (Signature'First + 2),
                    Element_Signature =>
                      Intern
                        (Signature
                           (Signature'First + 3 .. Signature'Last - 1)))
               do
                  Dict'Read (Stream, Temp);
               end return;
            when others =>
               return
                 Temp : D_Array
                   (Intern (Signature (Signature'First + 1 .. Signature'Last)))
               do
                  D_Array'Read (Stream, Temp);
               end return;
         end case;
      when others =>
         raise Constraint_Error;
   end case;
end D_Bus.Types.Dispatching_Read;
