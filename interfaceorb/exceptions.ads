-----------------------------------------------------------------------
----                                                               ----
----                  AdaBroker                                    ----
----                                                               ----
----     This package deals with the raising of C exceptions in    ----
----   Ada and ada ones in C.                                      ----
----     It is both a C and a Ada class (see Ada_Exceptions.hh)    ----
----   and provides 2 mains methods : raise_C_Exception and        ----
----   raise_Ada_Exception. The first one is called by Ada code    ----
----   and implemented in C. The second is called by C code and    ----
----   implemented in Ada. Both translate exceptions in the other  ----
----   language.                                                   ----
----                                                               ----
----                  package omniRopeAndKey                       ----
----                                                               ----
----   authors : Sebastien Ponce                                   ----
----   date    : 03/04/99                                          ----
----                                                               ----
----                                                               ----
-----------------------------------------------------------------------



package Exceptions is

   procedure C_Raise_C_UNKNOWN_Exception (Interfaces.C.Unsigned_Long Pd_Minor ;
                                          Interfaces.C.Int Pd_Status) ;
   pragma Import (Cpp,C_Raise_C_UNKNOWN_Exception,"");
   -- Wrapped around C function Raise_C_UNKNOWN_Exception
   -- declared in Ada_exceptions.hh
   -- Called by Ada code.
   -- Handles in C a Corba exception that was raised in Ada.

   procedure Raise_C_UNKNOWN_Exception (Corba.Unsigned_Long Pd_Minor ;
                                        Completion_Status Pd_Status) ;
   -- Ada equivalent of C procedure C_Raise_C_Exception

   procedure C_Raise_Ada_UNKNOWN_Exception (Interfaces.C.Unsigned_Long Pd_Minor ;
                                            Interfaces.C.Int Pd_Status) ;
   pragma Export (Cpp,C_Raise_Ada_Exception,"");
   -- Wrapped around C function Raise_Ada_UNKNOWN_Exception
   -- declared in Ada_exceptions.hh
   -- Called by C code.
   -- Handles in Ada a Corba exception that was raised in C.

   procedure Raise_Ada_UNKNOWN_Exception (Unknown_Members Member) ;
   -- Ada equivalent of C procedure C_Raise_Ada_Exception


private

   function Int_To_Status (Interfaces.C.Int N)
                           return Completion_Status ;

   function Status_To_Int (Completion_Status Status)
                           return Interfaces.C.Int ;

end Exceptions ;
