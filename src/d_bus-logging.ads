pragma Ada_2012;

with Ada.Exceptions;

package D_Bus.Logging is
   type Log_Severity is (Info, Warning, Error);
   --  Log severity types:
   --  Info     = Only printed if debugging requested by user.
   --  Warning  = Printed by default.
   --  Error    = Printed by default in red.

   procedure Log (Severity : Log_Severity; Message : String);
   procedure Log
     (Severity : Log_Severity; X : Ada.Exceptions.Exception_Occurrence);
   --  Send out a log message. This takes the following format:
   --  [D-Bus] [`Severity`] (`Address`) `Message/X`
end D_Bus.Logging;
