pragma Ada_2012;

with Ada.Strings.Unbounded;
with Ada.Characters.Handling;

with GNAT.Regexp;
with Interfaces;

package body D_Bus.Encodings is
   pragma Assertion_Policy (Static_Predicate => Check);
   pragma Assertion_Policy (Dynamic_Predicate => Check);

   ---------------
   -- Constants --
   ---------------
   Address_Safe_Bytes : constant GNAT.Regexp.Regexp :=
     GNAT.Regexp.Compile ("[\-0-9A-Za-z_/\.\*]");

   --------------
   -- Internal --
   --------------
   subtype Hex_Octet is Character with
       Static_Predicate => Hex_Octet in '0' .. '9' or Hex_Octet in 'a' .. 'f';
   subtype Numeric_Hex_Octet is Interfaces.Unsigned_8 range 16#0# .. 16#F#;

   -----------------------
   -- Conversion Tables --
   -----------------------
   type To_Octet_A is array (Numeric_Hex_Octet) of Hex_Octet;
   type To_Numeric_A is array (Character) of Numeric_Hex_Octet;

   pragma Style_Checks (Off);
   To_Octet   : constant To_Octet_A   :=
     ('0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'a', 'b', 'c', 'd',
      'e', 'f');
   To_Numeric : constant To_Numeric_A :=
     ('0' => 0, '1' => 1, '2' => 2, '3' => 3, '4' => 4, '5' => 5, '6' => 6,
      '7' => 7, '8' => 8, '9' => 9, 'a' => 10, 'b' => 11, 'c' => 12, 'd' => 13,
      'e' => 14, 'f' => 15, others => 0);
   pragma Style_Checks (On);

   subtype Hex_Byte is String (1 .. 2) with
       Dynamic_Predicate =>
        Hex_Byte (1) in Hex_Octet and Hex_Byte (2) in Hex_Octet;

   function To_Hex (Byte : Character) return Hex_Byte;
   function To_Hex (Byte : Character) return Hex_Byte is
      use type Interfaces.Unsigned_8;

      Pos    : constant Interfaces.Unsigned_8 := Character'Pos (Byte);
      Octet1 : constant Hex_Octet             :=
        To_Octet (Interfaces.Shift_Right (Pos, 4));
      Octet2 : constant Hex_Octet             := To_Octet (Pos and 16#0F#);
   begin
      return Octet1 & Octet2;
   end To_Hex;

   function From_Hex (Byte : Hex_Byte) return Character;
   function From_Hex (Byte : Hex_Byte) return Character is
      use type Interfaces.Unsigned_8;

      --  First and second octets
      O1 : constant Numeric_Hex_Octet := To_Numeric (Byte (1));
      O2 : constant Numeric_Hex_Octet := To_Numeric (Byte (2));
   begin
      --  Check octet validity
      if Byte (1) not in Hex_Octet or Byte (2) not in Hex_Octet then
         raise Invalid_Encoding;
      end if;

      return Character'Val (Interfaces.Shift_Left (O1, 4) or O2);
   end From_Hex;

   --------------------
   -- Implementation --
   --------------------
   function Encode_Server_Address (Text : String) return String is
      use Ada.Strings.Unbounded;

      Buffer : Ada.Strings.Unbounded.Unbounded_String;
   begin
      for Char of Text loop
         --  If the character does not need to be escaped, add directly
         if GNAT.Regexp.Match (String'(1 => Char), Address_Safe_Bytes) then
            Append (Buffer, Char);
         else
            Append (Buffer, "%" & Encodings.To_Hex (Char));
         end if;
      end loop;

      return To_String (Buffer);
   end Encode_Server_Address;

   function Decode_Server_Address (Text : String) return String is
      use Ada.Strings.Unbounded;

      Buffer : Ada.Strings.Unbounded.Unbounded_String;
      Index  : Positive := Text'First;
   begin
      if Text'Length = 0 then
         raise Invalid_Encoding;
      end if;

      while Index <= Text'Last loop
         case Text (Index) is
            when '%' =>
               --  Ensure there is sufficient space for the two hex bytes
               if Index + 2 > Text'Last then
                  raise Invalid_Encoding
                    with "Incomplete escaped byte found. Abort decode.";
               end if;

               --  Add From_Hex (%XX) and advance the index
               Append
                 (Buffer,
                  Character'(From_Hex (Text (Index + 1 .. Index + 2))));
               Index := Index + 3;
            when others =>
               if not GNAT.Regexp.Match
                   (String'(1 => Text (Index)), Address_Safe_Bytes)
               then
                  raise Invalid_Encoding
                    with "Unrecognised byte found while decoding string.";
               end if;

               Append (Buffer, Text (Index));

               Index := Index + 1;
         end case;
      end loop;

      return To_String (Buffer);
   end Decode_Server_Address;

   function To_Hex (Text : String) return String is
      Result     : String (1 .. Text'Length * 2);
      Normalised : constant String (1 .. Text'Length) := Text;
      --  Explicitly set bounds 1 .. X
      --  This makes the computations much easier
   begin
      for I in Normalised'Range loop
         Result (I * 2 - 1 .. I * 2) := To_Hex (Normalised (I));
      end loop;

      return Result;
   end To_Hex;

   function From_Hex (Text : String) return String is
      Result     : String (1 .. Text'Length / 2);
      Normalised : constant String (1 .. Text'Length) :=
        Ada.Characters.Handling.To_Lower (Text);
      --  See above (normalise bounds)
   begin
      for I in Result'Range loop
         Result (I) := From_Hex (Normalised (I * 2 - 1 .. I * 2));
      end loop;

      return Result;
   end From_Hex;
end D_Bus.Encodings;
