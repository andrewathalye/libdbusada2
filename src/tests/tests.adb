pragma Ada_2022;

with D_Bus.Connection;

procedure Tests is
begin
   declare
      C : D_Bus.Connection.Connection :=
        D_Bus.Connection.Connect ("autolaunch:");
   begin
      D_Bus.Connection.Disconnect (C);
   end;
end Tests;
