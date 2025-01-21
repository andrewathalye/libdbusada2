pragma Ada_2012;

with Ada.Containers.Indefinite_Holders;
with Ada.Strings.UTF_Encoding.Wide_Wide_Strings;

package D_Bus.Types.Basic_Generic is
   --  Generic packages for creating basic types

   -----------------
   -- Fixed Types --
   -----------------
   generic
      Type_Code : Signature_Element;
      type Inner is private;

      with function Image (X : Inner) return String;
   package Fixed_Wrappers is
      type Outer is new Basic_Type with private;

      function "+" (X : Inner) return Outer;
      function "+" (X : Outer) return Inner;

      overriding function Image (X : Outer) return String is (Image (+X));
   private
      package Padded_Inners is new Padded_Types (Inner, Inner'Size / 8);
      type Outer is new Basic_Type with record
         I : Padded_Inners.Padded_Type;
      end record;

      overriding function Signature (X : Outer) return Single_Signature is
        (1 => Type_Code);

      use type Ada.Streams.Stream_Element_Offset;
      overriding function Size
        (X : Outer) return Ada.Streams.Stream_Element_Count is
        (Inner'Size / 8);

      function "+" (X : Inner) return Outer is
        ((I => Padded_Inners.Padded_Type (X)));

      function "+" (X : Outer) return Inner is (Inner (X.I));
   end Fixed_Wrappers;

   generic
      Type_Code : Signature_Element;
      type Inner is (<>);
   package Discrete_Wrappers is
      type Outer is new Basic_Type with private with
        Integer_Literal => Value, String_Literal => Value;

      function "+" (X : Inner) return Outer;
      function "+" (X : Outer) return Inner;

      function Value (X : String) return Outer is (+Inner'Value (X));
      function Value (X : Wide_Wide_String) return Outer is
        (+Inner'Wide_Wide_Value (X));

      overriding function Image (X : Outer) return String;
   private
      package Fixed_Types is new Fixed_Wrappers
        (Type_Code, Inner, Inner'Image);
      type Outer is new Fixed_Types.Outer with null record;

      overriding function Signature (X : Outer) return Single_Signature is
        (Fixed_Types.Outer (X).Signature);

      overriding function Image (X : Outer) return String is
        (Fixed_Types.Outer (X).Image);

      use type Fixed_Types.Outer;
      function "+" (X : Inner) return Outer is
        (Fixed_Types."+" (X) with null record);
      function "+" (X : Outer) return Inner is (+Fixed_Types.Outer (X));
   end Discrete_Wrappers;

   generic
      Type_Code : Signature_Element;
      type Inner is digits <>;
   package Real_Wrappers is
      type Outer is new Basic_Type with private with
        Real_Literal => Value;

      function "+" (X : Inner) return Outer;
      function "+" (X : Outer) return Inner;

      function Value (X : String) return Outer is (+Inner'Value (X));

      overriding function Image (X : Outer) return String;
   private
      package Fixed_Types is new Fixed_Wrappers
        (Type_Code, Inner, Inner'Image);
      type Outer is new Fixed_Types.Outer with null record;

      overriding function Signature (X : Outer) return Single_Signature is
        (Fixed_Types.Outer (X).Signature);

      overriding function Image (X : Outer) return String is
        (Fixed_Types.Outer (X).Image);

      use type Fixed_Types.Outer;
      function "+" (X : Inner) return Outer is
        (Fixed_Types."+" (X) with null record);
      function "+" (X : Outer) return Inner is (+Fixed_Types.Outer (X));
   end Real_Wrappers;

   ------------------
   -- String Types --
   ------------------
   generic
      Type_Code : Signature_Element;
      type Data_Length_Type is mod <>;
      --  Note: must not be larger than 64 bits
      type External_Type is new String;
   package String_Wrappers is
      type Outer is new Basic_Type with private with
        String_Literal => Value;

      function "+" (X : Outer) return External_Type;
      function "+" (X : External_Type) return Outer;

      overriding function Image (X : Outer) return String;
      function Value (X : Wide_Wide_String) return Outer is
        (+External_Type
           (Ada.Strings.UTF_Encoding.UTF_8_String'
              (Ada.Strings.UTF_Encoding.Wide_Wide_Strings.Encode (X))));
   private
      package Padded_Data_Length_Types is new Padded_Types
        (Data_Length_Type, Data_Length_Type'Size / 8);

      type Internal_Raw_String is
        array (Padded_Data_Length_Types.Padded_Type range <>) of Character;
      type Internal_Type (L : Padded_Data_Length_Types.Padded_Type) is record
         S : Internal_Raw_String (1 .. L) := (others => ' ');
         C : Character                    := ASCII.NUL;
      end record;

      Empty_Internal_Type : constant Internal_Type :=
        (L => 0, S => <>, C => <>);

      package ITH is new Ada.Containers.Indefinite_Holders (Internal_Type);

      type Outer is new Basic_Type with record
         I : ITH.Holder := ITH.To_Holder (Empty_Internal_Type);
      end record;

      procedure Read
        (Stream :     not null access Ada.Streams.Root_Stream_Type'Class;
         Item   : out Outer);

      procedure Write
        (Stream : not null access Ada.Streams.Root_Stream_Type'Class;
         Item   : Outer);

      for Outer'Read use Read;
      for Outer'Write use Write;

      overriding function Signature (X : Outer) return Single_Signature is
        (1 => Type_Code);

      overriding function Size
        (X : Outer) return Ada.Streams.Stream_Element_Count;
   end String_Wrappers;
end D_Bus.Types.Basic_Generic;
