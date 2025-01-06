pragma Ada_2012;

with GNATCOLL.Strings;
with Interfaces;

package body D_Bus.Type_Internals.Containers is

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

      ----------
      -- Read --
      ----------
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
              (Dispatching_Read (Stream, Types (I).To_String));
         end loop;
      end Read;

      -----------
      -- Write --
      -----------
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

      ----------
      -- Size --
      ----------
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

      -----------
      -- Image --
      -----------
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

      ---------
      -- Set --
      ---------
      overriding procedure Set
        (Container : out Struct;
         Index : Natural; Value : Root_Type'Class)
      is
      begin
         Type_Check (Index, Value.Signature);
         Container.Inner (Index).Replace_Element (Value);
      end Set;

   end Structs;

   -----------------
   -- Has_Element --
   -----------------
   function Has_Element (C : Numeric_Container_Cursor) return Boolean is
   begin
      return C.Container.Has_Element (C.Index);
   end Has_Element;

   -----------
   -- Index --
   -----------
   function Index (C : Numeric_Container_Cursor) return Natural is
   begin
      return C.Index;
   end Index;

   -------------
   -- Element --
   -------------
   function Element (C : Numeric_Container_Cursor) return Root_Type'Class is
   begin
      return C.Container.Constant_Reference_NCT (C.Index).X.all;
   end Element;

   ------------
   -- Arrays --
   ------------
   package body Arrays is
      package Padded_Data_Lengths is new Padded_Types
        (Interfaces.Unsigned_32, 4);

      ----------
      -- Read --
      ----------

      procedure Read
        (Stream :     not null access Ada.Streams.Root_Stream_Type'Class;
         Item   : out D_Array)
      is
      begin
         pragma Compile_Time_Warning (Standard.True, "Read unimplemented");
         raise Program_Error with "Unimplemented procedure Read";
      end Read;

      -----------
      -- Write --
      -----------

      procedure Write
        (Stream : not null access Ada.Streams.Root_Stream_Type'Class;
         Item   : D_Array)
      is
      begin
         pragma Compile_Time_Warning (Standard.True, "Write unimplemented");
         raise Program_Error with "Unimplemented procedure Write";
      end Write;

      ----------
      -- Size --
      ----------
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

      -----------
      -- Image --
      -----------
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

      -----------
      -- First --
      -----------
      overriding function First
        (Object : D_Array) return Numeric_Container_Cursor
      is
      begin
         return (Container => Object'Unrestricted_Access,
                 Index => Object.Inner.First_Index);
      end First;

      ----------
      -- Next --
      ----------
      overriding function Next
        (Object : D_Array; Position : Numeric_Container_Cursor)
         return Numeric_Container_Cursor
      is
         New_Index : Integer := Position.Index + 1;
      begin
         if New_Index > Object.Inner.Last_Index then
            New_Index := No_Index;
         end if;

         return (Container => Object'Unrestricted_Access,
                 Index => New_Index);
      end Next;

      ----------
      -- Last --
      ----------
      overriding function Last
        (Object : D_Array) return Numeric_Container_Cursor
      is
      begin
         return (Container => Object'Unrestricted_Access,
                 Index => Object.Inner.Last_Index);
      end Last;

      --------------
      -- Previous --
      --------------
      overriding function Previous
        (Object : D_Array; Position : Numeric_Container_Cursor)
         return Numeric_Container_Cursor
      is
         New_Index : Integer := Position.Index - 1;
      begin
         if New_Index < Object.Inner.First_Index then
            New_Index := No_Index;
         end if;

         return (Container => Object'Unrestricted_Access,
                Index => New_Index);
      end Previous;

      ----------------------------
      -- Constant_Reference_NCT --
      ----------------------------
      overriding function Constant_Reference_NCT
        (Container : aliased D_Array;
         Index : Natural) return Constant_Reference_Type
      is
      begin
         return (X => Container.Inner.Constant_Reference
           (Index).Element.all'Unchecked_Access);
      end Constant_Reference_NCT;

      -------------------
      -- Reference_NCT --
      -------------------
      overriding function Reference_NCT
        (Container : aliased in out D_Array;
         Index : Natural) return Reference_Type
      is
      begin
         return (X => Container.Inner.Reference
           (Index).Element.all'Unchecked_Access);
      end Reference_NCT;

      -----------------
      -- Has_Element --
      -----------------
      overriding function Has_Element
        (Container : D_Array; Index : Natural) return Boolean
      is
      begin
         return Index
            in Container.Inner.First_Index .. Container.Inner.Last_Index;
      end Has_Element;

   end Arrays;

   -----------------
   -- Has_Element --
   -----------------

   function Has_Element (C : Keyed_Container_Cursor) return Boolean is
   begin
      pragma Compile_Time_Warning (Standard.True, "Has_Element unimplemented");
      return raise Program_Error with "Unimplemented function Has_Element";
   end Has_Element;

   ---------
   -- Key --
   ---------

   function Key (C : Keyed_Container_Cursor) return Basic_Type'Class is
   begin
      pragma Compile_Time_Warning (Standard.True, "Key unimplemented");
      return raise Program_Error with "Unimplemented function Key";
   end Key;

   -------------
   -- Element --
   -------------

   function Element (C : Keyed_Container_Cursor) return Root_Type'Class is
   begin
      pragma Compile_Time_Warning (Standard.True, "Element unimplemented");
      return raise Program_Error with "Unimplemented function Element";
   end Element;

   -----------
   -- Dicts --
   -----------

   package body Dicts is

      ----------
      -- Hash --
      ----------

      function Hash (Key : Key_Type) return Ada.Containers.Hash_Type is
      begin
         pragma Compile_Time_Warning (Standard.True, "Hash unimplemented");
         return raise Program_Error with "Unimplemented function Hash";
      end Hash;

      ----------
      -- Read --
      ----------

      procedure Read
        (Stream :     not null access Ada.Streams.Root_Stream_Type'Class;
         Item   : out Dict)
      is
      begin
         pragma Compile_Time_Warning (Standard.True, "Read unimplemented");
         raise Program_Error with "Unimplemented procedure Read";
      end Read;

      -----------
      -- Write --
      -----------

      procedure Write
        (Stream : not null access Ada.Streams.Root_Stream_Type'Class;
         Item   : Dict)
      is
      begin
         pragma Compile_Time_Warning (Standard.True, "Write unimplemented");
         raise Program_Error with "Unimplemented procedure Write";
      end Write;

      ------------
      -- Insert --
      ------------

      overriding procedure Insert
        (Container : in out Dict; Key : Basic_Type'Class;
         New_Item  :        Root_Type'Class)
      is
      begin
         pragma Compile_Time_Warning (Standard.True, "Insert unimplemented");
         raise Program_Error with "Unimplemented procedure Insert";
      end Insert;

      ----------
      -- Size --
      ----------

      overriding function Size
        (X : Dict) return Ada.Streams.Stream_Element_Count
      is
      begin
         pragma Compile_Time_Warning (Standard.True, "Size unimplemented");
         return raise Program_Error with "Unimplemented function Size";
      end Size;

      -----------
      -- Image --
      -----------

      overriding function Image (X : Dict) return String is
      begin
         pragma Compile_Time_Warning (Standard.True, "Image unimplemented");
         return raise Program_Error with "Unimplemented function Image";
      end Image;

      -----------
      -- First --
      -----------

      overriding function First (Object : Dict) return Keyed_Container_Cursor
      is
      begin
         pragma Compile_Time_Warning (Standard.True, "First unimplemented");
         return raise Program_Error with "Unimplemented function First";
      end First;

      ----------
      -- Next --
      ----------

      overriding function Next
        (Object : Dict; Position : Keyed_Container_Cursor)
         return Keyed_Container_Cursor
      is
      begin
         pragma Compile_Time_Warning (Standard.True, "Next unimplemented");
         return raise Program_Error with "Unimplemented function Next";
      end Next;

      ----------
      -- Last --
      ----------

      overriding function Last (Object : Dict) return Keyed_Container_Cursor is
      begin
         pragma Compile_Time_Warning (Standard.True, "Last unimplemented");
         return raise Program_Error with "Unimplemented function Last";
      end Last;

      --------------
      -- Previous --
      --------------

      overriding function Previous
        (Object : Dict; Position : Keyed_Container_Cursor)
         return Keyed_Container_Cursor
      is
      begin
         pragma Compile_Time_Warning (Standard.True, "Previous unimplemented");
         return raise Program_Error with "Unimplemented function Previous";
      end Previous;

      ----------------------------
      -- Constant_Reference_KCT --
      ----------------------------

      overriding function Constant_Reference_KCT
        (Container : aliased Dict; Key : Basic_Type'Class)
         return Constant_Reference_Type
      is
      begin
         pragma Compile_Time_Warning
           (Standard.True, "Constant_Reference_KCT unimplemented");
         return
           raise Program_Error
             with "Unimplemented function Constant_Reference_KCT";
      end Constant_Reference_KCT;

      -------------------
      -- Reference_KCT --
      -------------------

      overriding function Reference_KCT
        (Container : aliased in out Dict;
         Index : Basic_Type'Class) return Reference_Type
      is
      begin
         pragma Compile_Time_Warning
           (Standard.True, "Reference_KCT unimplemented");
         return
           raise Program_Error with "Unimplemented function Reference_KCT";
      end Reference_KCT;

   end Dicts;

   ---------
   -- "+" --
   ---------

   function "+" (X : Root_Type'Class) return Variant is
   begin
      pragma Compile_Time_Warning (Standard.True, """+"" unimplemented");
      return raise Program_Error with "Unimplemented function ""+""";
   end "+";

   ---------
   -- Get --
   ---------

   function Get (X : Variant) return Root_Type'Class is
   begin
      pragma Compile_Time_Warning (Standard.True, "Get unimplemented");
      return raise Program_Error with "Unimplemented function Get";
   end Get;

   ----------
   -- Size --
   ----------

   overriding function Size
     (X : Variant) return Ada.Streams.Stream_Element_Count
   is
   begin
      pragma Compile_Time_Warning (Standard.True, "Size unimplemented");
      return raise Program_Error with "Unimplemented function Size";
   end Size;

   -----------
   -- Image --
   -----------

   overriding function Image (X : Variant) return String is
   begin
      pragma Compile_Time_Warning (Standard.True, "Image unimplemented");
      return raise Program_Error with "Unimplemented function Image";
   end Image;

   ------------------
   -- Read_Variant --
   ------------------

   procedure Read_Variant
     (Stream :     not null access Ada.Streams.Root_Stream_Type'Class;
      Item   : out Variant)
   is
   begin
      pragma Compile_Time_Warning
        (Standard.True, "Read_Variant unimplemented");
      raise Program_Error with "Unimplemented procedure Read_Variant";
   end Read_Variant;

   -------------------
   -- Write_Variant --
   -------------------

   procedure Write_Variant
     (Stream : not null access Ada.Streams.Root_Stream_Type'Class;
      Item   : Variant)
   is
   begin
      pragma Compile_Time_Warning
        (Standard.True, "Write_Variant unimplemented");
      raise Program_Error with "Unimplemented procedure Write_Variant";
   end Write_Variant;

end D_Bus.Type_Internals.Containers;
