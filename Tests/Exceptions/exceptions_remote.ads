--
--  exceptions_remote.ads,v 1.2 1996/04/10 15:42:52 tardieu Exp
--

package Exceptions_Remote is

   pragma Remote_Call_Interface;

   User_Error : exception;

   procedure Raise_No_Error;
   procedure Raise_Program_Error;
   procedure Raise_Constraint_Error;
   procedure Raise_User_Error;

end Exceptions_Remote;
