pragma Ada_2022;

with Ada.Text_IO; use Ada.Text_IO;

with Ada.Containers.Indefinite_Vectors;
with GNAT.Regexp;
with GNATCOLL.Strings;

with D_Bus.Connection;

package body D_Bus.Types is
   -------------------------
   -- Signature Interning -- 
   -------------------------

   ----------------
   -- Signatures --
   ----------------
   function U_Split_Signature
     (X : U_Contents_Signature) return Single_Signature_Array;
   --  Exactly like the canonical `Split_Signature` but it
   --  performs no validation on `X`

   function U_Split_Signature
     (X : U_Contents_Signature) return Single_Signature_Array
   is
      X_USS : constant U_Single_Signature := U_Single_Signature (X);
      --  Note: This will often be INVALID

      function Read_Single_Signature
        (First : Positive; Last : out Positive) return Single_Signature;
      function Read_Single_Signature
        (First : Positive; Last : out Positive) return Single_Signature
      is
      begin
         Put_Line ("U_SS.RS: " & String (X (First .. X'Last)));
         if X (First) in Basic_Signature_Element or X (First) = Variant_CC then
            Last := First;
            return X_USS (First .. Last);
         end if;

         --  Non-solitary elements
         for I in First .. X'Last loop
            case X (I) is
               --  Add structs
               when Struct_Start_CC =>
                  --  Find terminating ')' or fail
                  declare
                     Paren_Count : Natural := 1;
                  begin
                     for J in I + 1 .. X'Last loop
                        case X (J) is
                           when Struct_Start_CC =>
                              Paren_Count := Paren_Count + 1;
                           when Struct_End_CC =>
                              Paren_Count := Paren_Count - 1;
                           when others =>
                              null;
                        end case;

                        if Paren_Count = 0 then
                           Last := J;
                           return X_USS (I .. J);
                        end if;
                     end loop;
                  end;
                  raise Constraint_Error;

                  --  Add arrays and dicts
               when Array_CC =>
                  --  Error if remaining length is too short for array
                  if First = X'Last then
                     raise Constraint_Error;
                  end if;

                  --  Check for dict
                  case X (I + 1) is
                     --  Dict
                     when Dict_Start_CC =>
                        --  Find terminating '}' or fail
                        declare
                           Bracket_Count : Natural := 1;
                        begin
                           for J in I + 2 .. X'Last loop
                              case X (J) is
                                 when Dict_Start_CC =>
                                    Bracket_Count := Bracket_Count + 1;
                                 when Dict_End_CC =>
                                    Bracket_Count := Bracket_Count - 1;
                                 when others =>
                                    null;
                              end case;

                              if Bracket_Count = 0 then
                                 Last := J;
                                 return X_USS (I .. J);
                              end if;
                           end loop;
                           raise Constraint_Error;
                        end;

                        --  Normal Array, checked via recursive call
                     when others =>
                        return
                          Array_CC &
                          Read_Single_Signature (First => I + 1, Last => Last);
                  end case;
                  --  No other valid elements
               when others =>
                  raise Constraint_Error;
            end case;
         end loop;

         return raise Program_Error;
      end Read_Single_Signature;

      package Single_Signature_Vectors is new Ada.Containers.Indefinite_Vectors
        (Positive, Single_Signature);

      --  Variables
      Result_Vector : Single_Signature_Vectors.Vector;
      First         : Positive := X'First;
      Last          : Natural  := 0;
   begin
      Put_Line ("U_SS: " & String (X));

      --  Read all single signatures
      while Last < X'Last loop
         Result_Vector.Append (Read_Single_Signature (First, Last));
         First := Last + 1;
      end loop;

      --  Produce and return array
      declare
         Result :
           Single_Signature_Array (1 .. Positive (Result_Vector.Length));
      begin
         for I in Result'Range loop
            Result (I) := Intern (Result_Vector (I));
         end loop;

         return Result;
      end;
   end U_Split_Signature;

   function Split_Signature
     (X : Contents_Signature) return Single_Signature_Array is
     (U_Split_Signature (X));

   function Validate_Single_Signature (X : U_Single_Signature) return Boolean
   is
   begin
      --  A type must not be empty
      if X'Length = 0 then
         return False;
      end if;

      --  Basic types can’t be longer than 1
      if X (X'First) in Basic_Signature_Element or X (X'First) = Variant_CC
      then
         if X'Length > 1 then
            return False;
         end if;

         return True;
      end if;

      --  Length check
      if X'Length > 255 then
         return False;
      end if;

      --  Full check of complex types
      case X (X'First) is
         when Array_CC =>
            --  Minimum length
            if X'Length < 2 then
               return False;
            end if;

            --  Check dicts
            if X (X'First + 1) = Dict_Start_CC then
               --  Must be 4 long and end in '}'
               if X'Length < 4 and X (X'Last) /= Dict_End_CC then
                  return False;
               end if;

               --  Key type must be BASIC
               if X (X'First + 2) not in Basic_Signature_Element then
                  return False;
               end if;

               --  Recursively check inner signature (only one content)
               return
                 Validate_Single_Signature (X (X'First + 3 .. X'Last - 1));
            end if;

            --  Check arrays via conversion
            declare
               SSA : constant Single_Signature_Array :=
                 U_Split_Signature
                   (U_Contents_Signature (X (X'First + 1 .. X'Last)));
               pragma Unreferenced (SSA);
            begin
               null;
            exception
               when Constraint_Error =>
                  return False;
            end;
         when Struct_Start_CC =>
            --  Type must be at least three long and end in ')'
            if X'Length < 3 or X (X'Last) /= Struct_End_CC then
               return False;
            end if;

            --  Check contents via conversion
            declare
               SSA : constant Single_Signature_Array :=
                 U_Split_Signature
                   (U_Contents_Signature (X (X'First + 1 .. X'Last - 1)));
               pragma Unreferenced (SSA);
            begin
               null;
            exception
               when Constraint_Error =>
                  return False;
            end;
         when others =>
            return False;
      end case;

      return True;
   end Validate_Single_Signature;

   function Validate_Contents_Signature
     (X : U_Contents_Signature) return Boolean
   is
   begin
      --  Length check
      if X'Length > 255 then
         return False;
      end if;

      --  The conversion actually performs the check
      --  Note: We use the unchecked variant to avoid an infinite loop
      declare
         SSA : constant Single_Signature_Array := U_Split_Signature (X);
         pragma Unreferenced (SSA);
      begin
         null;
      end;

      return True;
   exception
      when Constraint_Error =>
         return False;
   end Validate_Contents_Signature;

   -------------------------
   -- Root_Type Classwide --
   -------------------------
   function "=" (L, R : Root_Type'Class) return Boolean is
      use type Ada.Streams.Stream_Element_Count;
   begin
      if L.Signature /= R.Signature then
         return False;
      end if;

      if L.Size /= R.Size then
         return False;
      end if;

      return L.Image = R.Image;
   end "=";

   --------------------
   -- Argument Lists --
   --------------------
   function Signature (X : Argument_List) return Contents_Signature is
      Buf : GNATCOLL.Strings.XString;
   begin
      for Arg of X loop
         Buf.Append (String (Arg.Signature));
      end loop;

      return Contents_Signature (Buf.To_String);
   end Signature;

   function Size (X : Argument_List) return Ada.Streams.Stream_Element_Count is
      use type Ada.Streams.Stream_Element_Count;
      Counter : Ada.Streams.Stream_Element_Count := 0;
   begin
      for Element of X loop
         Counter := Counter + Element.Size;
      end loop;
      return Counter;
   end Size;

   ------------------
   -- Padded Types --
   ------------------
   package body Padded_Types is
      function To_ASA
        (Stream : not null access Ada.Streams.Root_Stream_Type'Class)
         return D_Bus.Connection.Alignable_Stream_Access;
      function To_ASA
        (Stream : not null access Ada.Streams.Root_Stream_Type'Class)
         return D_Bus.Connection.Alignable_Stream_Access
      is
      begin
         return D_Bus.Connection.Alignable_Stream (Stream.all)'Access;
      end To_ASA;

      ----------
      -- Read --
      ----------
      procedure Read
        (Stream :     not null access Ada.Streams.Root_Stream_Type'Class;
         Item   : out Padded_Type)
      is
         use type Ada.Streams.Stream_Element_Count;

         ASA : constant D_Bus.Connection.Alignable_Stream_Access :=
           To_ASA (Stream);

         Remainder : constant Ada.Streams.Stream_Element_Count :=
           ASA.Read_Count mod
           Ada.Streams.Stream_Element_Count (Alignment_Bytes);

         Discard : Character;
      begin
         for I in 1 .. Remainder loop
            Character'Read (Stream, Discard);
         end loop;
         --  Note: We should enforce these are null, but there is no
         --  security risk and the performance cost is unnecessarily high

         Base_Type'Read (Stream, Base_Type (Item));
      end Read;

      -----------
      -- Write --
      -----------
      procedure Write
        (Stream : not null access Ada.Streams.Root_Stream_Type'Class;
         Item   : Padded_Type)
      is
         use type Ada.Streams.Stream_Element_Count;

         ASA : constant D_Bus.Connection.Alignable_Stream_Access :=
           To_ASA (Stream);

         Remainder : constant Ada.Streams.Stream_Element_Count :=
           ASA.Write_Count mod
           Ada.Streams.Stream_Element_Count (Alignment_Bytes);
      begin
         for I in 1 .. Remainder loop
            Character'Write (Stream, ASCII.NUL);
         end loop;

         Base_Type'Write (Stream, Base_Type (Item));
      end Write;
   end Padded_Types;
end D_Bus.Types;
