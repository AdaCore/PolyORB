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

with Interfaces.C ;
with System ;

package Corba.Command_Line is

   -- this function rebuilts the C command line
   -- be careful :
   -- if the program's name is myprog,
   -- and the command line is "myprog -verbose"
   -- this function returns both myprog and -verbose
   procedure Get_Command_Line(C_Argc : out Interfaces.C.Int ;
                              C_Argv : out System.Address) ;

end Corba.Command_Line ;
