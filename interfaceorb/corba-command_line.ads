with Ada.Command_Line; use Ada.Command_Line;
with Interfaces.C;     use Interfaces.C;
with System;

package CORBA.Command_Line is

   Argc : constant Interfaces.C.int := Interfaces.C.int (Argument_Count + 1);
   --  Pseudo int that can be given as argument to a C function

   function Argv return System.Address;
   --  Pseudo char** that can be given as argument to a C function

private

   Pd_Argv : System.Address;

   function Get_Command_Line return System.Address;

end CORBA.Command_Line;
