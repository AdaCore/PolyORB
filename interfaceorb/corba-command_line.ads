-----------------------------------------------------------------------
----                                                               ----
----                  AdaBroker                                    ----
----                                                               ----
----              package Corba.Command_Line                       ----
----                                                               ----
----      This package allows to transforms the Ada command        ----
----      line arguments into C's argc and argv                    ----
----                                                               ----
----   authors : Fabien Azavant                                    ----
----   date    : 03/05/99                                          ----
----                                                               ----
----                                                               ----
-----------------------------------------------------------------------

with Ada.Command_Line ;  use Ada.Command_Line ;
with Interfaces.C ; use Interfaces.C ;
with System ;

package Corba.Command_Line is

   -- pseudo int that can be given as argument
   -- to a C function
   Argc : constant Interfaces.C.Int := Interfaces.C.Int(Argument_Count+1) ;


   -- pseudo char** that can be given as argument
   -- to a C function
   function Argv return System.Address ;


private

   Pd_Argv : System.Address  ;

   function Get_Command_Line return System.Address ;

end Corba.Command_Line ;
