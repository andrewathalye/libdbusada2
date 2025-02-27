pragma Ada_2012;

with Ada.Exceptions.Traceback;
with Ada.Strings.Unbounded;
with Ada.Text_IO;

with System.Address_Image;

with GNAT.Traceback;

with GNATCOLL.Terminal;

package body D_Bus.Logging is
   Debug : constant Boolean := True;
   --  TODO dynamically enable / disable

   ---------
   -- Log --
   ---------
   procedure Log
     (Severity : Log_Severity; Address : System.Address; Message : String);
   procedure Log
     (Severity : Log_Severity; Address : System.Address; Message : String)
   is
      use Ada.Strings.Unbounded;
      use type System.Address;

      Terminfo : GNATCOLL.Terminal.Terminal_Info;
      Display : Ada.Strings.Unbounded.Unbounded_String;
   begin
      --  Set colour and exit if needed
      case Severity is
         when Info =>
            if not Debug then
               return;
            end if;
         when Warning =>
            null;
         when Error =>
            GNATCOLL.Terminal.Init_For_Stderr (Terminfo);
            GNATCOLL.Terminal.Set_Fg (Terminfo, GNATCOLL.Terminal.Red);
      end case;

      Append (Display, "[D-Bus] [" & Severity'Image & "] ");

      --  Address info
      if Address /= System.Null_Address then
         Append (Display, "(");
         Append (Display, System.Address_Image (Address));
         Append (Display, ") ");
      end if;
      Append (Display, Message);

      Ada.Text_IO.Put_Line (Ada.Text_IO.Standard_Error, To_String (Display));

      case Severity is
         when Info | Warning =>
            null;
         when Error =>
            GNATCOLL.Terminal.Set_Fg (Terminfo, GNATCOLL.Terminal.Reset);
      end case;
   end Log;

   procedure Log (Severity : Log_Severity; Message : String) is
      TBA : GNAT.Traceback.Tracebacks_Array (1 .. 2);
      TB_Count : Natural;
   begin
      GNAT.Traceback.Call_Chain (TBA, TB_Count);

      if TB_Count = TBA'Last then
         Log
           (Severity, Ada.Exceptions.Traceback.Get_PC (TBA (TBA'Last)),
            Message);
      else
         Log (Severity, System.Null_Address, Message);
      end if;
   end Log;

   procedure Log
     (Severity : Log_Severity; X : Ada.Exceptions.Exception_Occurrence)
   is
      use Ada.Exceptions;

      TBA : constant GNAT.Traceback.Tracebacks_Array :=
        Ada.Exceptions.Traceback.Tracebacks (X);
   begin
      if TBA'Length /= 0 then
         Log
           (Severity, Ada.Exceptions.Traceback.Get_PC (TBA (TBA'First)),
            Exception_Name (X) & ": " & Exception_Message (X));
      else
         Log
           (Severity, System.Null_Address,
            Exception_Name (X) & ": " & Exception_Message (X));
      end if;
   end Log;

end D_Bus.Logging;
