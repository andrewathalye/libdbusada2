pragma Ada_2022;

with Ada.Containers.Indefinite_Hashed_Sets;
with Ada.Containers.Indefinite_Vectors;
with Ada.Strings.Hash;
with Ada.Unchecked_Conversion;

with GNATCOLL.Strings;

package body D_Bus.Types is
   -------------------------
   -- Signature Interning --
   -------------------------
   function Hash (Item : U_Contents_Signature) return Ada.Containers.Hash_Type;
   function Hash (Item : U_Contents_Signature) return Ada.Containers.Hash_Type
   is
   begin
      return Ada.Strings.Hash (String (Item));
   end Hash;

   package Signature_Sets is new
     Ada.Containers.Indefinite_Hashed_Sets (U_Contents_Signature, Hash, "=");

   type U_Contents_Signature_Access is access all U_Contents_Signature;

   Interned_Strings : Signature_Sets.Set;

   function Intern (X : Single_Signature) return Interned_Single_Signature is
      function Convert is new
        Ada.Unchecked_Conversion
          (U_Contents_Signature_Access,
           Interned_Single_Signature);

      Cursor : Signature_Sets.Cursor;
      Inserted : Boolean;
   begin
      Interned_Strings.Insert (U_Contents_Signature (X), Cursor, Inserted);
      return Convert (Interned_Strings.Constant_Reference (Cursor).Element);
   end Intern;

   function Intern (X : Contents_Signature) return Interned_Contents_Signature
   is
      function Convert is new
        Ada.Unchecked_Conversion
          (U_Contents_Signature_Access,
           Interned_Contents_Signature);

      Cursor : Signature_Sets.Cursor;
      Inserted : Boolean;
   begin
      Interned_Strings.Insert (X, Cursor, Inserted);
      return Convert (Interned_Strings.Constant_Reference (Cursor).Element);
   end Intern;

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
        (First : Positive; Last : out Positive) return Single_Signature is
      begin
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
                          Array_CC
                          & Read_Single_Signature
                              (First => I + 1, Last => Last);
                  end case;
                  --  No other valid elements

               when others =>
                  raise Constraint_Error;
            end case;
         end loop;

         return raise Program_Error;
      end Read_Single_Signature;

      package Single_Signature_Vectors is new
        Ada.Containers.Indefinite_Vectors (Positive, Single_Signature);

      --  Variables
      Result_Vector : Single_Signature_Vectors.Vector;
      First         : Positive := X'First;
      Last          : Natural := 0;
   begin
      --  Read all single signatures
      while Last < X'Last loop
         Result_Vector.Append (Read_Single_Signature (First, Last));
         First := Last + 1;
      end loop;

      --  Produce and return array
      --  Note: Ada 2022 syntax was the best I could come up with here
      --  Otherwise we'd break invariants for not null access
      return [for SS of Result_Vector => Intern (SS)];
   end U_Split_Signature;

   function Split_Signature
     (X : Contents_Signature) return Single_Signature_Array
   is (U_Split_Signature (X));

   function Validate_Single_Signature (X : U_Single_Signature) return Boolean
   is
   begin
      --  A type must not be empty
      if X'Length = 0 then
         return False;
      end if;

      --  Basic types can't be longer than 1
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
     (X : U_Contents_Signature) return Boolean is
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

      if L.Size (0) /= R.Size (0) then
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
         Counter := Counter + Element.Size (Counter);
      end loop;
      return Counter;
   end Size;

   -------------
   -- Padding --
   -------------
   function Alignment_For (CC : Signature_Element) return Padding_Alignment
   is (case CC is
         when Byte_CC => 1,
         when Boolean_CC => 4,
         when Int16_CC | Uint16_CC => 2,
         when Int32_CC | Uint32_CC => 4,
         when Int64_CC | Uint64_CC => 8,
         when Double_CC => 8,
         when String_CC => Alignment_For (Uint32_CC),
         when Object_Path_CC => Alignment_For (Uint32_CC),
         when Signature_CC => Alignment_For (Byte_CC),
         when Array_CC => Alignment_For (Uint32_CC),
         when Struct_Start_CC => 8,
         when Variant_CC => Alignment_For (Signature_CC),
         when File_Descriptor_CC => 4,
         when others => raise Program_Error);

end D_Bus.Types;
