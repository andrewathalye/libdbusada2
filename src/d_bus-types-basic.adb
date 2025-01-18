package body D_Bus.Types.Basic is
   ------------------
   -- Object Paths --
   ------------------
   --  From dbus-binding-generator-ada/share/introspect.xsd
   Object_Pattern : constant GNAT.Regexp.Regexp :=
     GNAT.Regexp.Compile
       (Pattern => "^\/?(([a-zA-Z0-9_])+(\/([a-zA-Z0-9_])+)?)+$|^\/$",
        Glob    => True, Case_Sensitive => True);

   -----------
   -- Valid --
   -----------
   function Validate_Object_Path (X : U_Object_Path) return Boolean is
   begin
      return GNAT.Regexp.Match (String (X), Object_Pattern);
   end Validate_Object_Path;
end D_Bus.Types.Basic;
