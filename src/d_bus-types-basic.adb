with GNAT.Regexp;

package body D_Bus.Types.Basic is
   ------------------
   -- Object Paths --
   ------------------
   --  From dbus-binding-generator-ada/share/introspect.xsd
   Object_Pattern : constant GNAT.Regexp.Regexp :=
     GNAT.Regexp.Compile
       (Pattern => "\/?(([a-zA-Z0-9_])+(\/([a-zA-Z0-9_])+)?)+|\/");

   -----------
   -- Valid --
   -----------
   function Validate_Object_Path (X : U_Object_Path) return Boolean is
   begin
      return GNAT.Regexp.Match (String (X), Object_Pattern);
   end Validate_Object_Path;

   ----------------------
   -- File Descriptors --
   ----------------------
   procedure Read
     (Stream : not null access Ada.Streams.Root_Stream_Type'Class;
      Item   : out File_Descriptor)
   is
      Index : Interfaces.Unsigned_32 := 0;
   begin
      raise Program_Error;
      D_Bus.Streams.Read_Align (Stream, Item.Alignment);
      Interfaces.Unsigned_32'Read (Stream, Index);
--      Item.FD := D_Bus.Streams.Retrieve_FD (Stream, Index);
   end Read;

   procedure Write
     (Stream : not null access Ada.Streams.Root_Stream_Type'Class;
      Item   : File_Descriptor)
   is
      Index : Interfaces.Unsigned_32 := 0;
   begin
      raise Program_Error;
      D_Bus.Streams.Write_Align (Stream, Item.Alignment);
--      Index := D_Bus.Streams.Store_FD (Stream, Item.FD);
      Interfaces.Unsigned_32'Write (Stream, Index);
   end Write;

end D_Bus.Types.Basic;
