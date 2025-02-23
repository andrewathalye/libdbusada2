pragma Ada_2012;

with Ada.Strings.Hash;
with Ada.Text_IO;
with D_Bus.Connection;
with GNATCOLL.Strings;
with Interfaces;

with D_Bus.Types.Basic;
with D_Bus.Types.Dispatching_Read;

package body D_Bus.Types.Containers is
   -------------------
   -- Type Checking --
   -------------------
   procedure Type_Check
     (Expected : Single_Signature; Found : Single_Signature);

   procedure Type_Check (Expected : Single_Signature; Found : Single_Signature)
   is
   begin
      if Expected /= Found then
         raise Constraint_Error
           with "Found " & String (Found) & ". Expected " & String (Expected);
      end if;
   end Type_Check;

   -------------
   -- Structs --
   -------------
   function "+" (Arguments : Argument_List) return Struct is
      I : Positive := 1;
   begin
      if Arguments.Is_Empty then
         raise Constraint_Error with "A Struct may not be empty";
      end if;

      return Result : Struct (Positive (Arguments.Length)) do
         for Argument of Arguments loop
            Result.Signatures (I) := Intern (Argument.Signature);
            Result.Elements (I).Replace_Element (Argument);

            I := I + 1;
         end loop;
      end return;
   end "+";

   function Is_Empty (Container : Struct) return Boolean is
   begin
      for Holder of Container.Elements loop
         if Holder.Is_Empty then
            return True;
         end if;
      end loop;

      return False;
   end Is_Empty;

   procedure Read
     (Stream :     not null access D_Bus.Connection.Alignable_Stream'Class;
      Item   : out Struct)
   is
   begin
      --  Padding
      D_Bus.Connection.Read_Align (Stream, Alignment_For (Struct_Start_CC));

      --  Read all elements
      for I in 1 .. Item.Count loop
         Item.Elements (I).Replace_Element
           (D_Bus.Types.Dispatching_Read (Stream, Item.Signatures (I).all));
      end loop;
   end Read;

   procedure Write
     (Stream : not null access D_Bus.Connection.Alignable_Stream'Class;
      Item   : Struct)
   is
   begin
      --  Padding
      D_Bus.Connection.Write_Align (Stream, Alignment_For (Struct_Start_CC));

      --  Write all elements
      for I of Item.Elements loop
         Root_Type'Class'Write (Stream, I.Element);
      end loop;
   end Write;

   function Empty (Signature : Contents_Signature) return Struct is
      Signatures : constant Single_Signature_Array :=
        Split_Signature (Signature);
   begin
      return
        Struct'
          (Count    => Signatures'Length, Signatures => Signatures,
           Elements => <>);
   end Empty;

   function Get (Container : Struct; Index : Positive) return Root_Type'Class
   is
   begin
      return Container.Elements (Index).Element;
   end Get;

   procedure Set
     (Container : out Struct; Index : Positive; Value : Root_Type'Class)
   is
   begin
      Type_Check (Container.Signatures (Index).all, Value.Signature);
      Container.Elements (Index).Replace_Element (Value);
   end Set;

   overriding function Size
     (X : Struct) return Ada.Streams.Stream_Element_Count
   is
      use type Ada.Streams.Stream_Element_Count;

      Buf : Ada.Streams.Stream_Element_Count := 0;
   begin
      for Holder of X.Elements loop
         Ada.Text_IO.Put_Line
           ("StructElem Size: " & Holder.Element.Size'Image);
         Buf := Buf + Holder.Element.Size;
      end loop;

      return Buf;
   end Size;
   --  TODO all these now need to include internal padding :(((

   overriding function Image (X : Struct) return String is
      Buf : GNATCOLL.Strings.XString;
   begin
      Buf.Append ("(");
      for Holder of X.Elements loop
         Buf.Append (Holder.Element.Image);
         Buf.Append (";");
      end loop;
      Buf.Append (")");

      return Buf.To_String;
   end Image;

   overriding function Contents (X : Struct) return Contents_Signature is
      Buf : GNATCOLL.Strings.XString;
   begin
      for Sig of X.Signatures loop
         Buf.Append (String (Sig.all));
      end loop;

      return Contents_Signature (Buf.To_String);
   end Contents;

   overriding function Signature (X : Struct) return Single_Signature is
   begin
      return
        Single_Signature
          (Struct_Start_CC & U_Single_Signature (X.Contents) & Struct_End_CC);
   end Signature;

   function Count (Container : Struct) return Positive is
   begin
      return Container.Count;
   end Count;

   ------------
   -- Arrays --
   ------------
   function Has_Element (Position : Array_Cursor) return Boolean is
   begin
      return Position /= No_Index;
   end Has_Element;

   function Index (Position : Array_Cursor) return Positive is
   begin
      return Position.Index;
   end Index;

   function Element (Position : Array_Cursor) return Root_Type'Class is
   begin
      return Position.Container.Inner (Position.Index);
   end Element;

   procedure Read
     (Stream :     not null access D_Bus.Connection.Alignable_Stream'Class;
      Item   : out D_Array)
   is
      use Ada.Streams;

      Length        : Interfaces.Unsigned_32;
      Stream_Length : Stream_Element_Count;
      Read_Count    : Stream_Element_Count := 0;
   begin
      --  Read size
      D_Bus.Connection.Read_Align (Stream, Alignment_For (Array_CC));
      Interfaces.Unsigned_32'Read (Stream, Length);
      Stream_Length := Ada.Streams.Stream_Element_Count (Length);

      --  Align for first element even in empty array
      D_Bus.Connection.Read_Align
        (Stream,
         Alignment_For
           (Item.Element_Signature.all (Item.Element_Signature.all'First)));

      --  Keep reading until size exhausted
      while Read_Count < Stream_Length loop
         declare
            Temp : constant D_Bus.Types.Root_Type'Class :=
              D_Bus.Types.Dispatching_Read
                (Stream, Item.Element_Signature.all);
         begin
            Read_Count := Read_Count + Temp.Size;
            Item.Inner.Append (Temp);
         end;
      end loop;
   end Read;

   procedure Write
     (Stream : not null access D_Bus.Connection.Alignable_Stream'Class;
      Item   : D_Array)
   is
      use Ada.Streams;

      Stream_Length : Stream_Element_Count := 0;
   begin
      --  Calculate size
      --  Note: We don't use .Size because that includes the Length itself
      for Element of Item.Inner loop
         Stream_Length := Stream_Length + Element.Size;
      end loop;

      --  Write size and array
      D_Bus.Connection.Write_Align (Stream, Alignment_For (Array_CC));
      Interfaces.Unsigned_32'Write (Stream, Stream_Length);

      --  Align even for empty array
      D_Bus.Connection.Write_Align
        (Stream,
         Alignment_For
           (Item.Element_Signature.all (Item.Element_Signature.all'First)));

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
      Counter := Interfaces.Unsigned_32'Size / 8;

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

   overriding function First (Object : D_Array) return Array_Cursor is
   begin
      return
        (Container => Object'Unrestricted_Access,
         Index     => Object.Inner.First_Index);
   end First;

   overriding function Next
     (Object : D_Array; Position : Array_Cursor) return Array_Cursor
   is
   begin
      if Position = No_Index then
         raise Constraint_Error;
      end if;

      if Position.Index = Object.Inner.Last_Index then
         return No_Index;
      end if;

      return
        (Container => Object'Unrestricted_Access, Index => Position.Index + 1);
   end Next;

   overriding function Last (Object : D_Array) return Array_Cursor is
   begin
      return
        (Container => Object'Unrestricted_Access,
         Index     => Object.Inner.Last_Index);
   end Last;

   overriding function Previous
     (Object : D_Array; Position : Array_Cursor) return Array_Cursor
   is
   begin
      if Position = No_Index then
         raise Constraint_Error;
      end if;

      if Position.Index = Object.Inner.First_Index then
         return No_Index;
      end if;

      return
        (Container => Object'Unrestricted_Access, Index => Position.Index - 1);
   end Previous;

   function Constant_Reference_A
     (Container : aliased D_Array; Index : Positive)
      return Constant_Reference_Type
   is
   begin
      return
        (X =>
           Container.Inner.Constant_Reference (Index).Element.all'
             Unchecked_Access);
   end Constant_Reference_A;

   function Constant_Reference_A
     (Container : aliased D_Array; Position : Array_Cursor)
      return Constant_Reference_Type is
     (Constant_Reference_A (Position.Container.all, Position.Index));

   function Reference_A
     (Container : aliased in out D_Array; Index : Positive)
      return Reference_Type
   is
   begin
      return
        (X => Container.Inner.Reference (Index).Element.all'Unchecked_Access);
   end Reference_A;

   function Reference_A
     (Container : aliased in out D_Array; Position : Array_Cursor)
      return Reference_Type is
     (Reference_A (Container, Position.Index));

   procedure Append (Container : in out D_Array; Element : Root_Type'Class) is
   begin
      Type_Check (Container.Element_Signature.all, Element.Signature);
      Container.Inner.Append (Element);
   end Append;

   overriding function Contents (X : D_Array) return Contents_Signature is
   begin
      return Contents_Signature (X.Element_Signature.all);
   end Contents;

   overriding function Signature (X : D_Array) return Single_Signature is
   begin
      return Array_CC & X.Element_Signature.all;
   end Signature;

   -----------
   -- Dicts --
   -----------
   function Has_Element (Position : Dict_Cursor) return Boolean is
   begin
      return Position /= No_Key;
   end Has_Element;

   function Key (Position : Dict_Cursor) return Basic_Type'Class is
   begin
      return Hash_Maps.Key (Hash_Maps.Cursor (Position));
   end Key;

   function Element (Position : Dict_Cursor) return Root_Type'Class is
   begin
      return Hash_Maps.Element (Hash_Maps.Cursor (Position));
   end Element;

   function Hash (Key : Basic_Type'Class) return Ada.Containers.Hash_Type is
   begin
      return Ada.Strings.Hash (Key.Image);
   end Hash;
   --  Note: this is an easy but not performant implementation

   procedure Read
     (Stream :     not null access D_Bus.Connection.Alignable_Stream'Class;
      Item   : out Dict)
   is
      use Ada.Streams;
      Stream_Length : Stream_Element_Count;
      Stream_Index  : Stream_Element_Count := 0;
   begin
      --  Read length
      Padded_Data_Lengths.Padded_Type'Read
        (Stream, Padded_Data_Lengths.Padded_Type (Stream_Length));

      --  Mandatory padding even for empty dict
      D_Bus.Connection.Read_Align (Stream, 8);

      --  Read keys and values
      while Stream_Index < Stream_Length loop
         --  Dict_Entry padding
         D_Bus.Connection.Read_Align (Stream, 8);

         declare
            Key   : constant Basic_Type'Class :=
              Basic_Type'Class
                (D_Bus.Types.Dispatching_Read
                   (Stream, (1 => Item.Key_Signature)));
            Value : constant Root_Type'Class  :=
              D_Bus.Types.Dispatching_Read
                (Stream, Item.Element_Signature.all);
         begin
            Item.Inner.Insert (Key, Value);
            Stream_Index := Stream_Index + Key.Size + Value.Size;
         end;
      end loop;
   end Read;

   procedure Write
     (Stream : not null access D_Bus.Connection.Alignable_Stream'Class; Item : Dict)
   is
      use Ada.Streams;
      Stream_Length : Stream_Element_Count := 0;
   begin
      --  Calculate Length
      --  Note: We don't use .Size because that includes the Length itself
      for C in Item.Inner.Iterate loop
         Stream_Length :=
           Stream_Length + Hash_Maps.Key (C).Size + Hash_Maps.Element (C).Size;
      end loop;

      D_Bus.Connection.Write_Align (Stream, Alignment_For (Array_CC));
      Interfaces.Unsigned_32'Write
        (Stream, Interfaces.Unsigned_32 (Stream_Length));

      D_Bus.Connection.Write_Align (Stream, 8);

      --  Write Key - Value pairs
      for C in Item.Inner.Iterate loop
         D_Bus.Connection.Write_Align (Stream, 8);
         Basic_Type'Class'Write (Stream, Hash_Maps.Key (C));
         Root_Type'Class'Write (Stream, Hash_Maps.Element (C));
      end loop;
   end Write;

   procedure Insert
     (Container : in out Dict; Key : Basic_Type'Class; Value : Root_Type'Class)
   is
   begin
      Type_Check ((1 => Container.Key_Signature), Key.Signature);
      Type_Check (Container.Element_Signature.all, Value.Signature);
      Container.Inner.Insert (Key, Value);
   end Insert;

   overriding function Size (X : Dict) return Ada.Streams.Stream_Element_Count
   is
      use Ada.Streams;
      Counter : Stream_Element_Count :=
        Padded_Data_Lengths.Padded_Type'Size / 8;
   begin
      for C in X.Inner.Iterate loop
         Counter :=
           Counter + Hash_Maps.Key (C).Size + Hash_Maps.Element (C).Size;
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

   function Constant_Reference_D
     (Container : aliased Dict; Key : Basic_Type'Class)
      return Constant_Reference_Type
   is
   begin
      return
        (X =>
           Hash_Maps.Constant_Reference (Container.Inner, Key).Element.all'
             Unchecked_Access);
   end Constant_Reference_D;

   function Constant_Reference_D
     (Container : aliased Dict; Position : Dict_Cursor)
      return Constant_Reference_Type is
     (Constant_Reference_D
        (Container, Hash_Maps.Key (Hash_Maps.Cursor (Position))));

   function Reference_D
     (Container : aliased in out Dict; Key : Basic_Type'Class)
      return Reference_Type
   is
   begin
      return
        (X =>
           Hash_Maps.Reference (Container.Inner, Key).Element.all'
             Unchecked_Access);
   end Reference_D;

   function Reference_D
     (Container : aliased in out Dict; Position : Dict_Cursor)
      return Reference_Type is
     (Reference_D (Container, Hash_Maps.Key (Hash_Maps.Cursor (Position))));

   overriding function Contents (X : Dict) return Contents_Signature is
   begin
      return Contents_Signature (X.Key_Signature & X.Element_Signature.all);
   end Contents;

   overriding function Signature (X : Dict) return Single_Signature is
   begin
      return
        Array_CC & Dict_Start_CC & X.Key_Signature & X.Element_Signature.all &
        Dict_End_CC;
   end Signature;

   --------------
   -- Variants --
   --------------
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
      use type D_Bus.Types.Basic.D_Signature;
   begin
      --  Type specification is to help out the compiler.
      Ada.Text_IO.Put_Line
        ("VarSig Size " &
         D_Bus.Types.Basic.D_Signature'(+X.Contents).Size'Image);
      Ada.Text_IO.Put_Line ("VarCont Size " & X.I.Element.Size'Image);
      return
        D_Bus.Types.Basic.D_Signature'(+X.Contents).Size + X.I.Element.Size;
   end Size;

   Variant_Signature : constant Single_Signature := (1 => Variant_CC);
   overriding function Signature (X : Variant) return Single_Signature is
     (Variant_Signature);

   overriding function Image (X : Variant) return String is
   begin
      return "{" & X.I.Element.Image & "}";
   end Image;

   procedure Read
     (Stream :     not null access D_Bus.Connection.Alignable_Stream'Class;
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
             (Stream, Single_Signature (Contents_Signature'(+VS)));
      begin
         Item.I.Replace_Element (Element);
      end;
   end Read;

   procedure Write
     (Stream : not null access D_Bus.Connection.Alignable_Stream'Class;
      Item   : Variant)
   is
      use type D_Bus.Types.Basic.D_Signature;
   begin
      --  Write signature
      D_Bus.Types.Basic.D_Signature'Write (Stream, +Item.Contents);

      --  Write element
      Root_Type'Class'Write (Stream, Item);
      Root_Type'Class'Write (Stream, Item.I.Element);
   end Write;

end D_Bus.Types.Containers;
