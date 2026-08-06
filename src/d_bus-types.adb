pragma Ada_2022;

with Ada.Containers.Indefinite_Hashed_Sets;
with Ada.Containers.Indefinite_Vectors;
with Ada.Strings.Hash;
with Ada.Tags.Generic_Dispatching_Constructor;
with Ada.Unchecked_Conversion;

with D_Bus.Types.Containers;
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

      Cursor   : Signature_Sets.Cursor;
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

      Cursor   : Signature_Sets.Cursor;
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

                           when Struct_End_CC   =>
                              Paren_Count := Paren_Count - 1;

                           when others          =>
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

               when Array_CC        =>
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

                                 when Dict_End_CC   =>
                                    Bracket_Count := Bracket_Count - 1;

                                 when others        =>
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

                     when others        =>
                        return
                          Array_CC
                          & Read_Single_Signature
                              (First => I + 1, Last => Last);
                  end case;
               --  No other valid elements

               when others          =>
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
         when Array_CC        =>
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

         when others          =>
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


   -----------------------
   -- Generic Iteration --
   -----------------------
   procedure For_Each
     (X : in out Container_Type'Class; Apply : Apply_Procedure)
   is
      use D_Bus.Types.Containers;
   begin
      if X in Struct'Class then
         for I in 1 .. Struct'Class (X).Count loop
            declare
               Obj : Root_Type'Class := Struct'Class (X).Get (I);
            begin
               Apply (Obj);
               Struct'Class (X).Set (I, Obj);
            end;
         end loop;
      elsif X in D_Array'Class then
         for Obj of D_Array'Class (X) loop
            Apply (Obj);
         end loop;
      elsif X in Dict'Class then
         --  TODO may cause problems, check?
         for Cur in Dict'Class (X).Iterate loop
            declare
               K : Basic_Type'Class := Key (Cur);
               V : Root_Type'Class := Element (Cur);
            begin
               Apply (K);
               Apply (V);
               Dict'Class (X).Delete (Key (Cur));
               Dict'Class (X).Insert (K, V);
            end;
         end loop;
      elsif X in Variant'Class then
         --  TODO what if someone derives from Variant?
         declare
            Obj : Root_Type'Class := Variant'Class (X).Get;
         begin
            Apply (Obj);
            X := Container_Type'Class (+Obj);
         end;
      else
         raise Unsupported_Container;
      end if;
   end For_Each;

   -----------------------------
   -- Dispatching Constructor --
   -----------------------------
   function Construct is new
     Ada.Tags.Generic_Dispatching_Constructor
       (Root_Type,
        Single_Signature,
        Constructor);

   function Dispatching_Construct
     (Signature : Single_Signature) return Root_Type'Class
   is
      --  Correctly handle dicts
      function Calculate_Tag return String;
      function Calculate_Tag return String is
      begin
         if Signature'Length >= 2
           and then
             U_Single_Signature
               (Signature (Signature'First .. Signature'First + 1))
             = "a{"
         then
            return "a{";
         else
            return String (Signature (Signature'First .. Signature'First));
         end if;
      end Calculate_Tag;
   begin
      return
        Construct
          (Ada.Tags.Internal_Tag ("D_Bus_Type_" & Calculate_Tag),
           Intern (Signature).all'Unrestricted_Access);
   end Dispatching_Construct;

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

end D_Bus.Types;
