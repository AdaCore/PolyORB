with Interfaces.C;

package Sys_Dep is

   type C_Boolean is new $BOOLEAN_TYPE;

   C_True  : C_Boolean := C_Boolean (1);
   C_False : C_Boolean := C_Boolean (0);
   --  Definition of C_Boolean, the Ada equivalent of the C++ bool
   --  type. Needed to interface C functions in Ada It is supposed here
   --  that the C internal representation of type boolean need one
   --  Bytes. If it is not the case, an error will occur when compiling and
   --  executing Ada_sys_Dep.cc and you will never come to this file.

   function Boolean_C_To_Ada (C_Bool : C_Boolean) return Boolean;
   function Boolean_Ada_To_C (Bool : Boolean) return C_Boolean;
   --  Conversion of C_Boolean into Ada boolean and vice versa

end Sys_Dep;



