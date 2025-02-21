pragma Ada_2012;

function D_Bus.Connection.Try_Authenticate
  (Mode : Mode_Type; C : Connected_Connection) return Boolean
is
   pragma Unreferenced (Mode);
   pragma Unreferenced (C);
begin
   return raise Program_Error;
end D_Bus.Connection.Try_Authenticate;
