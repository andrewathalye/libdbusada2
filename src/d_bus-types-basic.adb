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
   procedure Redeem
     (X : in out File_Descriptor;
      S : not null access D_Bus.Streams.Possible_Alignable_Stream) is
   begin
      X := +D_Bus.Streams.Retrieve_FD (S, Natural (+X));
   end Redeem;

   procedure Store
     (X : in out File_Descriptor;
      S : not null access D_Bus.Streams.Possible_Alignable_Stream) is
   begin
      X := +GNAT.OS_Lib.File_Descriptor (D_Bus.Streams.Store_FD (S, +X));
   end Store;
   --  Store a file descriptor in a Stream and store its index in the object

end D_Bus.Types.Basic;
