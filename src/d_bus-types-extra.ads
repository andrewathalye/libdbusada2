package D_Bus.Types.Extra is
   --  Extra types that do not have a special
   --  over-the-wire format but are defined
   --  in the D-Bus specification.

   -----------------------
   -- Validity Checking --
   -----------------------
   function Valid_Interface (X : String) return Boolean;
   function Valid_Member (X : String) return Boolean;
   function Valid_Bus (X : String) return Boolean;

   function Valid_Address (X : String) return Boolean;
   --  Validate the format, but not contents, of a server address

   -----------
   -- Types --
   -----------
   subtype Interface_Name is String with
       Dynamic_Predicate => Valid_Interface (Interface_Name),
       Predicate_Failure => "Invalid interface name " & Interface_Name;

   subtype Error_Name is Interface_Name;
   --  TODO check whether regex inherits

   subtype Member_Name is String with
       Dynamic_Predicate => Valid_Member (Member_Name),
       Predicate_Failure => "Invalid member name " & Member_Name;

   subtype Bus_Name is String with
       Dynamic_Predicate => Valid_Bus (Bus_Name),
       Predicate_Failure => "Invalid bus name " & Bus_Name;

   subtype Server_Address is String with
       Dynamic_Predicate => Valid_Address (Server_Address),
       Predicate_Failure => "Invalid server address " & Server_Address;
   --  A D-Bus Server Address according to the specification.
   --  This may specify multiple concrete servers, the below
   --  subprograms will try them, one by one, and use the first
   --  valid one.

end D_Bus.Types.Extra;
