-----------------------------------------------------------------------
----                                                               ----
----                  package sys_dep                              ----
----                                                               ----
----                                                               ----
----   This package defines some system dependent types.           ----
----   The system dependence is mostly due to the C++              ----
----   interfacing.                                                ----
----   In fact, this file is not system dependant but his          ----
----   implementation (see sys_dep_before_preprocessor.adb).       ----
----                                                               ----
----                                                               ----
----   authors : Sebastien Ponce                                   ----
----   date    : 02/17/99                                          ----
----                                                               ----
----                                                               ----
-----------------------------------------------------------------------

with Interfaces.C ;

package Sys_Dep is

   subtype C_Boolean is Interfaces.C.Unsigned_char ;
   C_True : C_Boolean := 1 ;
   C_False : C_Boolean := 0 ;
   --    Definition of C_Boolean, the Ada equivalent of the C++
   -- bool type. Needed to interface C functions in Ada
   --    It is supposed here that the C internal representation of type
   -- boolean need one Byte. If it is not the case, an error will occur
   -- when compiling and executing Ada_sys_Dep.cc and you will never come
   -- to this file.

   function Boolean_C_To_Ada (C_Bool : C_Boolean) return Boolean;
   function Boolean_Ada_To_C (Bool : Boolean) return C_Boolean;
   -- conversion of C_Boolean into Ada boolean and vice versa


end Sys_Dep ;



