-----------------------------------------------------------------------
----                                                               ----
----                  AdaBroker                                    ----
----                                                               ----
----                  package CORBA.Boa                            ----
----                                                               ----
----   authors : Sebastien Ponce, Fabien Azavant                   ----
----   date    :                                                   ----
----                                                               ----
----                                                               ----
-----------------------------------------------------------------------



package Corba.Boa is


   --------------------------------------------------
   ---        not specified in CORBA2.2          ----
   --------------------------------------------------

   procedure Init(Orb_Name : in String) ;
   -- wrapper around BOA_ptr CORBA::ORB::BOA_init(int& argc, char** argv,
   --                               const char* boa_identifier);
   -- in CORBA.h L 2092


private



end Corba.Boa ;
