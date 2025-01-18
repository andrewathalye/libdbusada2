pragma Ada_2012;

with Ada.Containers.Indefinite_Holders;

package D_Bus.Types.Basic_Generic is
   --  Generic packages for creating basic types
   --  TODO literal support?

   -----------------
   -- Fixed Types --
   -----------------
   --  Note: `Inner` must not be larger than 64 bits
   generic
      Type_Code : Signature_Element;
      type Inner is private;
   package Fixed_Wrappers is
      type Outer is new Basic_Type with private;

      function "+" (X : Inner) return Outer;
      function "+" (X : Outer) return Inner;

      overriding
      function Image (X : Outer) return String;
   private
      package Padded_Inners is new Padded_Types (Inner, Inner'Size / 8);
      type Outer is new Basic_Type with record
         I : Padded_Inners.Padded_Type;
      end record;

      overriding
      function Signature (X : Outer) return Single_Signature
      is (1 => Type_Code);

      overriding
      function Size
        (X : Outer) return Ada.Streams.Stream_Element_Count is (Inner'Size);
   end Fixed_Wrappers;

   ------------------
   -- String Types --
   ------------------
   generic
      Type_Code : Signature_Element;
      type Data_Length_Type is mod <>;
      type External_Type is new String;
   package String_Wrappers is
      type Outer is new Basic_Type with private;

      function "+" (X : Outer) return External_Type;
      function "+" (X : External_Type) return Outer;

      overriding
      function Image (X : Outer) return String;
   private
      package Padded_Data_Length_Types
      is new Padded_Types (Data_Length_Type, Data_Length_Type'Size / 8);

      type Internal_Raw_String is array
        (Padded_Data_Length_Types.Padded_Type range <>) of Character;
      type Internal_Type (L : Padded_Data_Length_Types.Padded_Type) is record
         S : Internal_Raw_String (1 .. L) := (others => ' ');
         C : Character := ASCII.NUL;
      end record;
      --  TODO check whether calling 'Output and 'Input does what we want
      Empty_Internal_Type : constant Internal_Type :=
        (L => 0, S => <>, C => <>);

      package ITH is new Ada.Containers.Indefinite_Holders (Internal_Type);

      type Outer is new Basic_Type
      with record
         I : ITH.Holder := ITH.To_Holder (Empty_Internal_Type);
      end record;

      procedure Read
        (Stream : not null access Ada.Streams.Root_Stream_Type'Class;
         Item : out Outer);

      procedure Write
        (Stream : not null access Ada.Streams.Root_Stream_Type'Class;
         Item : Outer);

      for Outer'Read use Read;
      for Outer'Write use Write;

      overriding
      function Signature
        (X : Outer) return Single_Signature
      is (1 => Type_Code);

      overriding
      function Size
        (X : Outer) return Ada.Streams.Stream_Element_Count;
   end String_Wrappers;
end D_Bus.Types.Basic_Generic;
