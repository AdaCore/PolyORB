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


with Interfaces.C ;
with Corba ;
with Ada.Exceptions ;

package Exceptions is

   procedure C_Raise_C_UNKNOWN_Exception (Pd_Minor : in Interfaces.C.Unsigned_Long ;
                                          Pd_Status : in Interfaces.C.Int) ;
   pragma Import (Cpp,C_Raise_C_UNKNOWN_Exception,"Raise_C_UNKNOWN_Exception__FUlQ25CORBA16CompletionStatus");
   -- Wrapped around C function Raise_C_UNKNOWN_Exception
   -- declared in Ada_exceptions.hh
   -- Called by Ada code.
   -- Handles in C a Corba exception that was raised in Ada.

   procedure Raise_C_UNKNOWN_Exception (E : in Ada.Exceptions.Exception_Occurrence) ;
   -- Ada equivalent of C procedure C_Raise_C_Exception

   procedure C_Raise_Ada_UNKNOWN_Exception (Pd_Minor : in Interfaces.C.Unsigned_Long ;
                                            Pd_Status : in Interfaces.C.Int) ;
   pragma Export (Cpp,C_Raise_Ada_UNKNOWN_Exception,"Raise_Ada_UNKNOWN_Exception__FUlQ25CORBA16CompletionStatus");
   -- Wrapped around C function Raise_Ada_UNKNOWN_Exception
   -- declared in Ada_exceptions.hh
   -- Called by C code.
   -- Handles in Ada a Corba exception that was raised in C.

   procedure Raise_Ada_UNKNOWN_Exception (Pd_Minor : in Corba.Unsigned_Long ;
                                          Pd_Status : in Corba.Completion_Status) ;
   -- Ada equivalent of C procedure C_Raise_Ada_Exception



private

   function Int_To_Status (N : in Interfaces.C.Int)
                           return Corba.Completion_Status ;

   function Status_To_Int (Status : in Corba.Completion_Status)
                           return Interfaces.C.Int ;

end Exceptions ;
