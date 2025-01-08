pragma Ada_2012;

with Ada.Strings.Hash;
with Ada.Tags;
with Ada.Text_IO;
with GNATCOLL.Strings;
with Interfaces;

with D_Bus.Types.Basic;
with D_Bus.Types.Dispatching_Read;

package body D_Bus.Types.Containers is
   -------------
   -- Structs --
   -------------
   package body Structs is
      procedure Type_Check
        (Index : Natural;
         Signature : Single_Signature);
      procedure Type_Check
        (Index : Natural;
         Signature : Single_Signature)
      is
      begin
         if Types (Index).To_String /= Signature then
            raise Constraint_Error with String (Types (Index).To_String)
               & " /= " & String (Signature);
         end if;
      end Type_Check;

      type Struct_NR is null record;
      package Struct_Padding is new Padded_Types (Struct_NR, 8);

      procedure Read
        (Stream :     not null access Ada.Streams.Root_Stream_Type'Class;
         Item   : out Struct)
      is
         Discard : Struct_Padding.Padded_Type;
      begin
         --  Padding
         Struct_Padding.Padded_Type'Read (Stream, Discard);

         --  Read all elements
         for I in Types'Range loop
            Item.Inner (I).Replace_Element
              (D_Bus.Types.Dispatching_Read
                (Stream, Types (I).To_String));
               --  TODO start using To_String?
         end loop;
      end Read;

      procedure Write
        (Stream : not null access Ada.Streams.Root_Stream_Type'Class;
         Item   : Struct)
      is
         Padding : Struct_Padding.Padded_Type;
      begin
         --  Padding
         Struct_Padding.Padded_Type'Write (Stream, Padding);

         --  Write all elements
         for I of Item.Inner loop
            Root_Type'Class'Write (Stream, I.Element);
         end loop;
      end Write;

      overriding function Size
        (X : Struct) return Ada.Streams.Stream_Element_Count
      is
         use type Ada.Streams.Stream_Element_Count;

         Buf : Ada.Streams.Stream_Element_Count := 0;
      begin
         for Holder of X.Inner loop
            Buf := Buf + Holder.Element.Size;
         end loop;

         return Buf;
      end Size;

      overriding function Image (X : Struct) return String is
         Buf : GNATCOLL.Strings.XString;
      begin
         Buf.Append ("(");
         for Holder of X.Inner loop
            Buf.Append (Holder.Element.Image);
            Buf.Append (";");
         end loop;
         Buf.Append (")");

         return Buf.To_String;
      end Image;

      overriding procedure Set
        (Container : out Struct;
         Index : Natural; Value : Root_Type'Class)
      is
      begin
         Type_Check (Index, Value.Signature);
         Container.Inner (Index).Replace_Element (Value);
      end Set;

   end Structs;

   ------------------------
   -- Numeric Containers --
   ------------------------
   function Has_Element (C : Numeric_Container_Cursor) return Boolean is
   begin
      return C.Container.Has_Element (C.Index);
   end Has_Element;

   function Index (C : Numeric_Container_Cursor) return Natural is
   begin
      return C.Index;
   end Index;

   function Element (C : Numeric_Container_Cursor) return Root_Type'Class is
   begin
      return C.Container.Constant_Reference_NCT (C.Index).X.all;
   end Element;

   -------------------------
   -- Padded Data Lengths --
   -------------------------
   package Padded_Data_Lengths is new Padded_Types
        (Interfaces.Unsigned_32, 4);

   ------------
   -- Arrays --
   ------------
   package body Arrays is
      procedure Read
        (Stream :     not null access Ada.Streams.Root_Stream_Type'Class;
         Item   : out D_Array)
      is
         use Ada.Streams;

         Length : Padded_Data_Lengths.Padded_Type;
         Stream_Length : Stream_Element_Count;
         Read_Count : Stream_Element_Count := 0;
      begin
         --  Read size
         Padded_Data_Lengths.Padded_Type'Read (Stream, Length);
         Stream_Length := Ada.Streams.Stream_Element_Count (Length);

         --  Keep reading until size exhausted
         while Read_Count < Stream_Length loop
            declare
               Temp : constant D_Bus.Types.Root_Type'Class :=
                  D_Bus.Types.Dispatching_Read (Stream, Inner_Signature);
            begin
               Read_Count := Read_Count + Temp.Size;
               Item.Inner.Append (Temp);
            end;
         end loop;
      end Read;

      procedure Write
        (Stream : not null access Ada.Streams.Root_Stream_Type'Class;
         Item   : D_Array)
      is
         use Ada.Streams;

         Stream_Length : Stream_Element_Count := 0;
      begin
         --  Calculate size
         --  Note: We don’t use .Size because that includes the Length itself
         for Element of Item.Inner loop
            Stream_Length := Stream_Length + Element.Size;
         end loop;

         --  Write size and array
         Padded_Data_Lengths.Padded_Type'Write
           (Stream, Padded_Data_Lengths.Padded_Type (Stream_Length));

         for Element of Item.Inner loop
            Root_Type'Class'Write (Stream, Element);
         end loop;
      end Write;

      overriding function Size
        (X : D_Array) return Ada.Streams.Stream_Element_Count
      is
         use type Ada.Streams.Stream_Element_Count;

         Counter : Ada.Streams.Stream_Element_Count;
      begin
         --  Base size
         Counter := Padded_Data_Lengths.Padded_Type'Size / 8;

         for Element of X.Inner loop
            Counter := Counter + Element.Size;
         end loop;

         return Counter;
      end Size;

      overriding function Image (X : D_Array) return String is
         Buf : GNATCOLL.Strings.XString;
      begin
         Buf.Append ("[");

         for Element of X.Inner loop
            Buf.Append (Element.Image);
            Buf.Append (";");
         end loop;

         Buf.Append ("]");

         return Buf.To_String;
      end Image;

      overriding function First
        (Object : D_Array) return Numeric_Container_Cursor
      is
      begin
         return (Container => Object'Unrestricted_Access,
                 Index => Object.Inner.First_Index);
      end First;

      overriding function Next
        (Object : D_Array; Position : Numeric_Container_Cursor)
         return Numeric_Container_Cursor
      is
      begin
         if Position = No_Index then
            raise Constraint_Error;
         end if;

         if Position.Index = Object.Inner.Last_Index then
            return No_Index;
         end if;

         return (Container => Object'Unrestricted_Access,
                 Index => Position.Index + 1);
      end Next;

      overriding function Last
        (Object : D_Array) return Numeric_Container_Cursor
      is
      begin
         return (Container => Object'Unrestricted_Access,
                 Index => Object.Inner.Last_Index);
      end Last;

      overriding function Previous
        (Object : D_Array; Position : Numeric_Container_Cursor)
         return Numeric_Container_Cursor
      is
      begin
         if Position = No_Index then
            raise Constraint_Error;
         end if;

         if Position.Index = Object.Inner.First_Index then
            return No_Index;
         end if;

         return (Container => Object'Unrestricted_Access,
                Index => Position.Index - 1);
      end Previous;

      overriding function Constant_Reference_NCT
        (Container : aliased D_Array;
         Index : Natural) return Constant_Reference_Type
      is
      begin
         return (X => Container.Inner.Constant_Reference
           (Index).Element.all'Unchecked_Access);
      end Constant_Reference_NCT;

      overriding function Constant_Reference_NCT
        (Container : aliased D_Array;
         C : Numeric_Container_Cursor) return Constant_Reference_Type
      is (Constant_Reference_NCT (C.Container.all, C.Index));

      overriding function Reference_NCT
        (Container : aliased in out D_Array;
         Index : Natural) return Reference_Type
      is
      begin
         return (X => Container.Inner.Reference
           (Index).Element.all'Unchecked_Access);
      end Reference_NCT;

      overriding function Reference_NCT
        (Container : aliased in out D_Array;
         C : Numeric_Container_Cursor) return Reference_Type
      is (Reference_NCT (Container, C.Index));

      overriding function Has_Element
        (Container : D_Array; Index : Natural) return Boolean
      is
      begin
         return Index
            in Container.Inner.First_Index .. Container.Inner.Last_Index;
      end Has_Element;

      overriding procedure Append
        (Container : out D_Array;
         Element : Root_Type'Class)
      is
      begin
         if Element.Signature /= Inner_Signature then
            raise Constraint_Error;
         end if;

         Container.Inner.Append (Element);
      end Append;

   end Arrays;

   ----------------------
   -- Keyed Containers --
   ----------------------
   function Has_Element (C : Keyed_Container_Cursor) return Boolean is
   begin
      return C.Container.Has_Element (C.Key.all);
   end Has_Element;

   function Key (C : Keyed_Container_Cursor) return Basic_Type'Class is
   begin
      if C.Key = null then
         raise Constraint_Error;
      end if;

      return C.Key.all;
   end Key;

   function Element (C : Keyed_Container_Cursor) return Root_Type'Class is
   begin
      return C.Container.Constant_Reference_KCT (C.Key.all).X.all;
   end Element;

   package body Dicts is
      type NR is null record;
      package Padded_NRs is new Padded_Types (NR, 8);
      Padded_NR : constant Padded_NRs.Padded_Type := (null record);
      --  8 bytes of padding required before dict_entry
      --  but we don’t actually want to implement the full type because
      --  it would lead to code bloat and security risks

      procedure Type_Check
        (Key : Basic_Type'Class;
         Value : Root_Type'Class);

      procedure Type_Check
        (Key : Basic_Type'Class;
         Value : Root_Type'Class)
      is
      begin
         if Key.Signature /= (1 => Key_Type_Code)
            or Value.Signature /= Value_Signature
         then
            raise Constraint_Error with "WRONG SIGNATURE TODO";
         end if;
      end Type_Check;

      function Hash (Key : Basic_Type'Class) return Ada.Containers.Hash_Type is
      begin
         return Ada.Strings.Hash (Key.Image);
      end Hash;
      --  Note: this is an easy but not performant implementation

      procedure Read
        (Stream :     not null access Ada.Streams.Root_Stream_Type'Class;
         Item   : out Dict)
      is
         use Ada.Streams;
         Stream_Length : Stream_Element_Count;
         Stream_Index : Stream_Element_Count := 0;
      begin
         --  Read length
         Padded_Data_Lengths.Padded_Type'Read
           (Stream, Padded_Data_Lengths.Padded_Type (Stream_Length));


         --  Add keys and values
         while Stream_Index < Stream_Length loop
            declare
               Key : constant Basic_Type'Class :=
                 Basic_Type'Class
                   (D_Bus.Types.Dispatching_Read
                     (Stream, (1 => Key_Type_Code)));
               Value : constant Root_Type'Class :=
                 D_Bus.Types.Dispatching_Read (Stream, Value_Signature);
            begin
               Item.Inner.Insert (Key, Value);
               Stream_Index := Stream_Index + Key.Size + Value.Size;
            end;
         end loop;
      end Read;

      procedure Write
        (Stream : not null access Ada.Streams.Root_Stream_Type'Class;
         Item   : Dict)
      is
         use Ada.Streams;
         Stream_Length : Stream_Element_Count := 0;
      begin
         --  Calculate Length
         --  Note: We don’t use .Size because that includes the Length itself
         for C in Item.Inner.Iterate loop
            Stream_Length := Stream_Length + Hash_Maps.Key (C).Size
               + Hash_Maps.Element (C).Size;
         end loop;

         --  Write Length
         Padded_Data_Lengths.Padded_Type'Write
           (Stream,
            Padded_Data_Lengths.Padded_Type (Item.Size));

         --  Write Key - Value pairs
         for C in Item.Inner.Iterate loop
            Padded_NRs.Padded_Type'Write (Stream, Padded_NR);
            Basic_Type'Class'Write (Stream, Hash_Maps.Key (C));
            Root_Type'Class'Write (Stream, Hash_Maps.Element (C));
         end loop;
      end Write;

      overriding procedure Insert
        (Container : in out Dict; Key : Basic_Type'Class;
         Value  :        Root_Type'Class)
      is
      begin
         Ada.Text_IO.Put_Line ("INSERT");
         Ada.Text_IO.Put_Line (Key.Image);
         Ada.Text_IO.Put_Line (Ada.Tags.Expanded_Name (Value'Tag));
         Ada.Text_IO.Put_Line (Numeric_Container_Type'Class (Value).Image);

         Type_Check (Key, Value);
         Container.Inner.Insert (Key, Value);
      end Insert;

      overriding function Size
        (X : Dict) return Ada.Streams.Stream_Element_Count
      is
         use Ada.Streams;
         Counter : Stream_Element_Count :=
            Padded_Data_Lengths.Padded_Type'Size / 8;
      begin
         for C in X.Inner.Iterate loop
            Counter := Counter + Hash_Maps.Key (C).Size
               + Hash_Maps.Element (C).Size;
         end loop;

         return Counter;
      end Size;

      overriding function Image (X : Dict) return String is
         Buf : GNATCOLL.Strings.XString;
      begin
         Buf.Append ("[");
         for C in X.Inner.Iterate loop
            Buf.Append (Hash_Maps.Key (C).Image);
            Buf.Append (" => ");
            Buf.Append (Hash_Maps.Element (C).Image);
            Buf.Append (";");
         end loop;
         Buf.Append ("]");
         return Buf.To_String;
      end Image;

      overriding function First (Object : Dict) return Keyed_Container_Cursor
      is
         use type Hash_Maps.Cursor;
         FEC : constant Hash_Maps.Cursor := Object.Inner.First;
         --  First Element Cursor
      begin
         if FEC = Hash_Maps.No_Element then
            return No_Key;
         end if;

         return (Container => Object'Unchecked_Access,
                 Key => Hash_Maps.Key (FEC)'Unrestricted_Access);
         --  Note: UNRESTRICTED because ALIASED not given
      end First;

      overriding function Next
        (Object : Dict; Position : Keyed_Container_Cursor)
         return Keyed_Container_Cursor
      is
         use type Hash_Maps.Cursor;

         NPC : Hash_Maps.Cursor;
         --  Next Position Cursor
      begin
         if Position = No_Key then
            raise Constraint_Error;
         end if;

         NPC := Hash_Maps.Next
           (Object.Inner.Find (Position.Key.all));
         if NPC = Hash_Maps.No_Element then
            return No_Key;
         else
            return (Container => Object'Unchecked_Access,
                    Key => Hash_Maps.Key (NPC)'Unrestricted_Access);
            --  Note: unrestricted because `aliased` not given
         end if;
      end Next;

      overriding function Constant_Reference_KCT
        (Container : aliased Dict; Key : Basic_Type'Class)
         return Constant_Reference_Type
      is
      begin
         return (X =>
            Hash_Maps.Constant_Reference
              (Container.Inner, Key).Element.all'Unchecked_Access);
      end Constant_Reference_KCT;

      overriding function Constant_Reference_KCT
        (Container : aliased Dict; C : Keyed_Container_Cursor)
         return Constant_Reference_Type
      is (Constant_Reference_KCT (C.Container.all, C.Key.all));

      overriding function Reference_KCT
        (Container : aliased in out Dict;
         Key : Basic_Type'Class) return Reference_Type
      is
      begin
         return (X =>
            Hash_Maps.Reference
              (Container.Inner, Key).Element.all'Unchecked_Access);
      end Reference_KCT;

      overriding function Reference_KCT
        (Container : aliased in out Dict; C : Keyed_Container_Cursor)
         return Reference_Type
      is (Reference_KCT (Container, C.Key.all));
   end Dicts;

   --------------
   -- Variants --
   --------------
   -----------------------
   -- Variant_Signature --
   --      (private)    --
   -----------------------
   function Variant_Signature
     (X : Variant) return D_Bus.Types.Basic.D_Signature;
   function Variant_Signature
     (X : Variant) return D_Bus.Types.Basic.D_Signature
   is
      use type D_Bus.Types.Basic.D_Signature;
   begin
      return +X.Contents;
   end Variant_Signature;

   function "+" (X : Root_Type'Class) return Variant is
   begin
      return (I => Root_Type_Holders.To_Holder (X));
   end "+";

   function Get (X : Variant) return Root_Type'Class is
   begin
      return X.I.Element;
   end Get;

   overriding function Size
     (X : Variant) return Ada.Streams.Stream_Element_Count
   is
      use type Ada.Streams.Stream_Element_Count;
   begin
      --  Note: Cast is to help the compiler out
      --  There’s a lot of overloading here
      return Basic_Type'Class (Variant_Signature (X)).Size + X.I.Element.Size;
   end Size;

   overriding function Image (X : Variant) return String is
   begin
      return "{" & X.I.Element.Image & "}";
   end Image;

   procedure Read_Variant
     (Stream :     not null access Ada.Streams.Root_Stream_Type'Class;
      Item   : out Variant)
   is
      use type D_Bus.Types.Basic.D_Signature;

      VS : D_Bus.Types.Basic.D_Signature;
   begin
      --  Read signature
      D_Bus.Types.Basic.D_Signature'Read (Stream, VS);

      --  Read element
      declare
         Element : constant Root_Type'Class :=
            D_Bus.Types.Dispatching_Read
              (Stream,
               Single_Signature (Contents_Signature'(+VS)));
      begin
         Item.I.Replace_Element (Element);
      end;
   end Read_Variant;

   procedure Write_Variant
     (Stream : not null access Ada.Streams.Root_Stream_Type'Class;
      Item   : Variant)
   is
   begin
      --  Write signature
      D_Bus.Types.Basic.D_Signature'Write
        (Stream, Variant_Signature (Item));

      --  Write element
      Root_Type'Class'Write (Stream, Item.I.Element);
   end Write_Variant;

end D_Bus.Types.Containers;
