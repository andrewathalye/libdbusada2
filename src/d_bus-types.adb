pragma Ada_2022;

with Ada.Containers.Indefinite_Vectors;
with GNAT.Regexp;
with GNATCOLL.Strings;

with D_Bus.Connection;

package body D_Bus.Types is
   ------------------
   -- Object Paths --
   ------------------
   --  From dbus-binding-generator-ada/share/introspect.xsd
   Object_Pattern : constant GNAT.Regexp.Regexp := GNAT.Regexp.Compile
     (Pattern => "^\/?(([a-zA-Z0-9_])+(\/([a-zA-Z0-9_])+)?)+$|^\/$",
      Glob => True,
      Case_Sensitive => True);

   -----------
   -- Valid --
   -----------
   function Validate_Object_Path (X : U_Object_Path) return Boolean
   is
   begin
      return GNAT.Regexp.Match (String (X), Object_Pattern);
   end Validate_Object_Path;

   ----------------
   -- Signatures --
   ----------------
   subtype Sig_Solitary is Character
   with Static_Predicate =>
      Sig_Solitary in 'y' | 'b' | 'n' | 'q' | 'i' | 'u' | 'x' | 't' | 'd'
         | 'h' | 's' | 'o' | 'g' | 'v';
   --  Types which are only ever one type code long

   subtype Sig_Basic is Character
   with Static_Predicate =>
      Sig_Basic in 'y' | 'b' | 'n' | 'q' | 'i' | 'u' | 'x' | 't' | 'd'
         | 'h' | 's' | 'o' | 'g';
   --  Only basic D-Bus types

   function Split_Signature
     (X : Contents_Signature) return Single_Signature_Array
   is
      X_USS : constant U_Single_Signature := U_Single_Signature (X);
      --  Note: This will often be INVALID

      function Read_Single_Signature
        (First : Positive;
         Last : out Positive) return Single_Signature;
      function Read_Single_Signature
        (First : Positive;
         Last : out Positive) return Single_Signature
      is
      begin
         if X (First) in Sig_Solitary then
            Last := First;
            return X_USS (First .. First);
         end if;

         --  Non-solitary elements
         for I in X'Range loop
            case X (I) is
               --  Add structs
               when '(' =>
                  --  Find terminating ')' or fail
                  declare
                     Paren_Count : Natural := 1;
                  begin
                     for J in I + 1 .. X'Last loop
                        case X (J) is
                           when '(' =>
                              Paren_Count := Paren_Count + 1;
                           when ')' =>
                              Paren_Count := Paren_Count - 1;
                           when others => null;
                        end case;

                        if Paren_Count = 0 then
                           Last := J;
                           return X_USS (I .. J);
                        end if;
                     end loop;
                  end;
                  raise Constraint_Error;

               --  Add arrays and dicts
               when 'a' =>
                  --  Error if remaining length is too short for array
                  if First = X'Last then
                     raise Constraint_Error;
                  end if;

                  --  Check for dict
                  case X (I + 1) is
                     --  Dict
                     when '{' =>
                        --  Find terminating '}' or fail
                        declare
                           Bracket_Count : Natural := 1;
                        begin
                           for J in I + 2 .. X'Last loop
                              case X (J) is
                                 when '{' =>
                                    Bracket_Count := Bracket_Count + 1;
                                 when '}' =>
                                    Bracket_Count := Bracket_Count - 1;
                                 when others => null;
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
                        return "a" & Read_Single_Signature
                          (First => I + 1,
                           Last => Last);
                  end case;
               --  No other valid elements
               when others =>
                  raise Constraint_Error;
            end case;
         end loop;

         return raise Program_Error;
      end Read_Single_Signature;

      package Single_Signature_Vectors is new Ada.Containers.Indefinite_Vectors
        (Natural, Single_Signature);

      --  Variables
      Result_Vector : Single_Signature_Vectors.Vector;
      First : Positive := X'First;
      Last : Natural := 0;
   begin
      --  Read all single signatures
      while Last < X'Last loop
         Result_Vector.Append (Read_Single_Signature (First, Last));
         First := Last + 1;
      end loop;

      --  Produce and return array
      declare
         Result : Single_Signature_Array
           (1 .. Positive (Result_Vector.Length));
      begin
         for I in Result'Range loop
            Result (I) := Unbounded_Single_Signatures.To_XString
              (Result_Vector (I));
         end loop;

         return Result;
      end;
   end Split_Signature;

   function Validate_Single_Signature (X : U_Single_Signature) return Boolean
   is
   begin
      --  Basic types can’t be longer than 1
      if X (X'First) in Sig_Solitary and X'Length = 1 then
         return True;
      end if;

      --  Length check
      if X'Length > 255 then
         return False;
      end if;

      --  Full check of complex types
      case X (X'First) is
         when 'a' =>
            --  Minimum length
            if X'Length < 2 then
               return False;
            end if;

            --  Check dicts
            if X (X'First + 1) = '{' then
               --  Must be 4 long and end in '}'
               if X'Length < 4 and X (X'Last) /= '}' then
                  return False;
               end if;

               --  Key type must be BASIC
               if X (X'First + 2) not in Sig_Basic then
                  return False;
               end if;

               --  Recursively check inner signature (only one content)
               return Validate_Single_Signature
                  (X (X'First + 3 .. X'Last - 1));
            end if;

            --  Check arrays via conversion
            declare
               SSA : constant Single_Signature_Array := Split_Signature
                 (Contents_Signature (X (X'First + 1 .. X'Last)));
               pragma Unreferenced (SSA);
            begin
               null;
            exception
               when Constraint_Error => return False;
            end;
         when '(' =>
            --  Type must be at least three long and end in ')'
            if X'Length < 3 or X (X'Last) /= ')' then
               return False;
            end if;

            --  Check contents via conversion
            declare
               SSA : constant Single_Signature_Array := Split_Signature
                 (Contents_Signature (X (X'First + 1 .. X'First - 1)));
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
      --  TODO can this cause an infinite loop?
      --  only one way to find out :)
      declare
         SSA : constant Single_Signature_Array := Split_Signature (X);
         pragma Unreferenced (SSA);
      begin
         null;
      end;

      return True;
   exception
      when Constraint_Error => return False;
   end Validate_Contents_Signature;

   -------------------------
   -- Root_Type Classwide --
   -------------------------
   function "=" (L, R : Root_Type'Class) return Boolean
   is
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
   function Signature (X : Argument_List) return Contents_Signature
   is
      Buf : GNATCOLL.Strings.XString;
   begin
      for Arg of X loop
         Buf.Append (String (Arg.Signature));
      end loop;

      return Contents_Signature (Buf.To_String);
   end Signature;

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
        (Stream : not null access Ada.Streams.Root_Stream_Type'Class;
         Item : out Padded_Type)
      is
         use type Ada.Streams.Stream_Element_Count;

         ASA : constant D_Bus.Connection.Alignable_Stream_Access :=
            To_ASA (Stream);

         Remainder : constant Ada.Streams.Stream_Element_Count :=
            ASA.Read_Count mod Ada.Streams.Stream_Element_Count
              (Alignment_Bytes);

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
         Item : Padded_Type)
      is
         use type Ada.Streams.Stream_Element_Count;

         ASA : constant D_Bus.Connection.Alignable_Stream_Access :=
            To_ASA (Stream);

         Remainder : constant Ada.Streams.Stream_Element_Count :=
           ASA.Write_Count mod Ada.Streams.Stream_Element_Count
             (Alignment_Bytes);
      begin
         for I in 1 .. Remainder loop
            Character'Write (Stream, ASCII.NUL);
         end loop;

         Base_Type'Write (Stream, Base_Type (Item));
      end Write;
   end Padded_Types;
end D_Bus.Types;
