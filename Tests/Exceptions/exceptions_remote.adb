--
--  exceptions_remote.adb,v 1.2 1996/04/10 15:42:51 tardieu Exp
--

with Ada.Exceptions; use Ada.Exceptions;

package body Exceptions_Remote is

   procedure Raise_No_Error is begin null; end Raise_No_Error;

   procedure Raise_Program_Error is
   begin
      Raise_Exception (Program_Error'Identity,
                       "This is the Program error message");
   end Raise_Program_Error;

   procedure Raise_Constraint_Error is
   begin
      Raise_Exception (Constraint_Error'Identity,
                       "This is the Constraint error message");
   end Raise_Constraint_Error;

   procedure Raise_User_Error is
   begin
      Raise_Exception (User_Error'Identity,
                       "This is the User error message");
   end Raise_User_Error;

end Exceptions_Remote;
