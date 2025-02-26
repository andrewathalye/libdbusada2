pragma Ada_2012;

with GNAT.Regexp;
with GNAT.Regpat;

package body D_Bus.Types.Extra is
   -----------------------
   -- Validity Checking --
   -----------------------
   --  From dbus-binding-generator-ada/share/introspect.xsd
   Interface_Regexp : constant GNAT.Regexp.Regexp :=
     GNAT.Regexp.Compile
       (Pattern => "([a-zA-Z_]([a-zA-Z0-9_])*\.)+[a-zA-Z_]([a-zA-Z0-9_])*");
   function Valid_Interface (X : String) return Boolean is
   begin
      return GNAT.Regexp.Match (X, Interface_Regexp) and X'Length < 256;
   end Valid_Interface;

   --  From dbus-binding-generator-ada/share/introspect.xsd
   Member_Regexp : constant GNAT.Regexp.Regexp :=
     GNAT.Regexp.Compile (Pattern => "([a-zA-Z0-9_])*");
   function Valid_Member (X : String) return Boolean is
   begin
      return GNAT.Regexp.Match (X, Member_Regexp) and X'Length < 256;
   end Valid_Member;

   Bus_Regexp : constant GNAT.Regexp.Regexp :=
     GNAT.Regexp.Compile
       (Pattern =>
          "((:[A-Za-z0-9])|([A-Za-z]))((\.)*[a-zA-Z0-9]([a-zA-Z0-9_-])*)*");
   function Valid_Bus (X : String) return Boolean is
   begin
      return GNAT.Regexp.Match (X, Bus_Regexp) and X'Length < 256;
   end Valid_Bus;

   pragma Style_Checks (Off);
   Server_Regpat : constant GNAT.Regpat.Pattern_Matcher :=
     GNAT.Regpat.Compile
       (Expression =>
          "(([a-z]+):(?:([a-z]+)=((?:[-0-9A-Za-z_\/.\*]|(?:%[0-9A-Fa-f]{2}))+)(?:,([a-z]+)=((?:[-0-9A-Za-z_\/.\*]|(?:%[0-9A-Fa-f]{2}))+))*)?)(?:;(([a-z]+):(?:([a-z]+)=((?:[-0-9A-Za-z_\/.\*]|(?:%[0-9A-Fa-f]{2}))+)(?:,([a-z]+)=((?:[-0-9A-Za-z_\/.\*]|(?:%[0-9A-Fa-f]{2}))+))*)?))*");
   --  Note: Generated from specification using tools/regex_serveraddr.sh
   --  This will recognise any SYNTACTICALLY valid server address
   pragma Style_Checks (On);

   function Valid_Address (X : String) return Boolean is
   begin
      return GNAT.Regpat.Match (Server_Regpat, X);
   end Valid_Address;

end D_Bus.Types.Extra;
