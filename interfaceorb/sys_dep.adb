with Interfaces.C; use Interfaces.C;
with Ada.Text_IO;

package body Sys_Dep is

   System_Dependant_Error : exception;
   --  This exception is raised if the configuration of the system is not
   --  suitable

   ----------------------
   -- Boolean_C_To_Ada --
   ----------------------

   function Boolean_C_To_Ada
     (C_Bool : C_Boolean)
      return Boolean
   is
   begin
      return C_Bool /= C_False;
   end Boolean_C_To_Ada;

   ----------------------
   -- Boolean_Ada_To_C --
   ----------------------

   function Boolean_Ada_To_C
     (Bool : Boolean)
      return C_Boolean
   is
   begin
      if Bool then
         return C_True;
      else
         return C_False;
      end if;
   end Boolean_Ada_To_C;

begin
   --  This code is executed when one declares a with clause for this
   --  library. It checks the system's configuration
   if Interfaces.C.UCHAR_MAX /= 255 then
      Ada.Text_IO.Put_Line ("Sys_Dep error : UCHAR_MAX /= 255");
      raise System_Dependant_Error;
      --  Raise exception if the size of Interfaces.C.unsigned_char is not
      --  8 bits. Needed to ensure compatibility between C++ and Ada
      --  definitions of the Corba type Octet
   end if;

end Sys_Dep;





