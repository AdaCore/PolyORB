-----------------------------------------------------------------------
----                                                               ----
----                  package sys_dep                              ----
----                                                               ----
----                                                               ----
----   This package defines some system dependent types.           ----
----   The system dependence is mostly due to the C++              ----
----   interfacing.                                                ----
----                                                               ----
----                                                               ----
----   authors : Sebastien Ponce                                   ----
----   date    : 02/17/99                                          ----
----                                                               ----
----                                                               ----
-----------------------------------------------------------------------


with Interfaces.C ; use Interfaces.C ;
with Ada.Text_IO ;

package body Sys_Dep is

   -- System_Dependant_Error
   -------------------------
   System_Dependant_Error : exception;
   -- this exception is raised if the configuration of the
   -- system is not suitable

   -- Boolean_C_To_Ada
   -------------------
   function Boolean_C_To_Ada (C_Bool : C_Boolean) return Boolean is
   begin
      return C_Bool /= C_False ;
      -- no use of C_True since every C_Boolean except 0 is true
      -- and not only C_True which value is 1
   end ;

   -- Boolean_Ada_To_C
   -------------------
   function Boolean_Ada_To_C (Bool : Boolean) return C_Boolean is
   begin
      if Bool
      then
         return C_True ;
      else
         return C_False ;
      end if ;
   end;


begin
   if Interfaces.C.UCHAR_MAX /= 255
   then
      Ada.Text_IO.Put_Line ("Error in package Sys_Dep :");
      Ada.Text_IO.Put_Line ("   The size of Ada char is not compatible with Corba.") ;
      Ada.Text_IO.Put_Line ("   Interfaces.C.UCHAR_MAX must be equal to 255.") ;
      raise System_Dependant_Error ;
      -- raise exception if the size of Interfaces.C.unsigned_char
      -- is not 8 bits. Needed to ensure compatibility between C++
      -- and Ada definitions of the Corba type Octet
   end if ;

#If HAS_Cplusplus_Bool
#else
   Ada.Text_IO.Put_Line ("Error in package Sys_Dep :");
   Ada.Text_IO.Put_Line ("   The system variable Has_Cplusplus_Bool does not") ;
   Ada.Text_IO.Put_Line ("   exist. Unable to compile. You have to change.") ;
   Ada.Text_IO.Put_Line ("   the definition of C_Boolean in Sys_Dep.ads.") ;
   raise System_Dependant_Error ;
#end if ;

end Sys_Dep ;

