-----------------------------------------------------------------------
-----------------------------------------------------------------------
----                                                               ----
----                         AdaBroker                             ----
----                                                               ----
----                   package Exceptions                          ----
----                                                               ----
----                                                               ----
----   Copyright (C) 1999 ENST                                     ----
----                                                               ----
----   This file is part of the AdaBroker library                  ----
----                                                               ----
----   The AdaBroker library is free software; you can             ----
----   redistribute it and/or modify it under the terms of the     ----
----   GNU Library General Public License as published by the      ----
----   Free Software Foundation; either version 2 of the License,  ----
----   or (at your option) any later version.                      ----
----                                                               ----
----   This library is distributed in the hope that it will be     ----
----   useful, but WITHOUT ANY WARRANTY; without even the implied  ----
----   warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR     ----
----   PURPOSE.  See the GNU Library General Public License for    ----
----   more details.                                               ----
----                                                               ----
----   You should have received a copy of the GNU Library General  ----
----   Public License along with this library; if not, write to    ----
----   the Free Software Foundation, Inc., 59 Temple Place -       ----
----   Suite 330, Boston, MA 02111-1307, USA                       ----
----                                                               ----
----                                                               ----
----                                                               ----
----   Description                                                 ----
----   -----------                                                 ----
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
----                                                               ----
----   authors : Sebastien Ponce, Fabien Azavant                   ----
----   date    : 02/28/99                                          ----
----                                                               ----
-----------------------------------------------------------------------
-----------------------------------------------------------------------


with Ada.Exceptions ;
with Interfaces.C ;
with Interfaces.C.Strings ;

with Corba ;

package Exceptions is

   ---------------------------------
   -- Handling of Fatal exception --
   ---------------------------------

   procedure C_Raise_Ada_Fatal_Exception (file : in Interfaces.C.Strings.Chars_Ptr ;
                                          Line : in Interfaces.C.Int ;
                                          Err_msg : in Interfaces.C.Strings.Chars_Ptr) ;
   pragma Export (Cpp,
                  C_Raise_Ada_Fatal_Exception,
                  "Raise_Ada_FatalException__FPCciT0");



   -----------------------------------
   -- Handling of UNKNOWN exception --
   -----------------------------------

   procedure C_Raise_Ada_UNKNOWN_Exception (Pd_Minor : in Interfaces.C.Unsigned_Long ;
                                            Pd_Status : in Interfaces.C.Int) ;
   pragma Export (Cpp,
                  C_Raise_Ada_UNKNOWN_Exception,
                  "Raise_Ada_UNKNOWN_Exception__FUlQ25CORBA16CompletionStatus");
   -- Wrapped around C function Raise_Ada_UNKNOWN_Exception
   -- declared in Ada_exceptions.hh
   -- Called by C code.
   -- Handles in Ada a Corba exception that was raised in C.



   -------------------------------------------------------
   -- And now the same methods for the other exceptions --
   -------------------------------------------------------

   procedure C_Raise_Ada_BAD_PARAM_Exception (Pd_Minor : in Interfaces.C.Unsigned_Long ;
                                              Pd_Status : in Interfaces.C.Int) ;
   pragma Export (Cpp,
                  C_Raise_Ada_BAD_PARAM_Exception,
                  "Raise_Ada_BAD_PARAM_Exception__FUlQ25CORBA16CompletionStatus");

   procedure C_Raise_Ada_NO_MEMORY_Exception (Pd_Minor : in Interfaces.C.Unsigned_Long ;
                                              Pd_Status : in Interfaces.C.Int) ;
   pragma Export (Cpp,
                  C_Raise_Ada_NO_MEMORY_Exception,
                  "Raise_Ada_NO_MEMORY_Exception__FUlQ25CORBA16CompletionStatus");

   procedure C_Raise_Ada_IMP_LIMIT_Exception (Pd_Minor : in Interfaces.C.Unsigned_Long ;
                                              Pd_Status : in Interfaces.C.Int) ;
   pragma Export (Cpp,
                  C_Raise_Ada_IMP_LIMIT_Exception,
                  "Raise_Ada_IMP_LIMIT_Exception__FUlQ25CORBA16CompletionStatus");

   procedure C_Raise_Ada_COMM_FAILURE_Exception (Pd_Minor : in Interfaces.C.Unsigned_Long ;
                                                 Pd_Status : in Interfaces.C.Int) ;
   pragma Export (Cpp,
                  C_Raise_Ada_COMM_FAILURE_Exception,
                  "Raise_Ada_COMM_FAILURE_Exception__FUlQ25CORBA16CompletionStatus");

   procedure C_Raise_Ada_INV_OBJREF_Exception (Pd_Minor : in Interfaces.C.Unsigned_Long ;
                                               Pd_Status : in Interfaces.C.Int) ;
   pragma Export (Cpp,
                  C_Raise_Ada_INV_OBJREF_Exception,
                  "Raise_Ada_INV_OBJREF_Exception__FUlQ25CORBA16CompletionStatus");

   procedure C_Raise_Ada_OBJECT_NOT_EXIST_Exception (Pd_Minor : in Interfaces.C.Unsigned_Long ;
                                                     Pd_Status : in Interfaces.C.Int) ;
   pragma Export (Cpp,
                  C_Raise_Ada_OBJECT_NOT_EXIST_Exception,
                  "Raise_Ada_OBJECT_NOT_EXIST_Exception__FUlQ25CORBA16CompletionStatus");

   procedure C_Raise_Ada_NO_PERMISSION_Exception (Pd_Minor : in Interfaces.C.Unsigned_Long ;
                                                  Pd_Status : in Interfaces.C.Int) ;
   pragma Export (Cpp,
                  C_Raise_Ada_NO_PERMISSION_Exception,
                  "Raise_Ada_NO_PERMISSION_Exception__FUlQ25CORBA16CompletionStatus");

   procedure C_Raise_Ada_INTERNAL_Exception (Pd_Minor : in Interfaces.C.Unsigned_Long ;
                                             Pd_Status : in Interfaces.C.Int) ;
   pragma Export (Cpp,
                  C_Raise_Ada_INTERNAL_Exception,
                  "Raise_Ada_INTERNAL_Exception__FUlQ25CORBA16CompletionStatus");

   procedure C_Raise_Ada_MARSHAL_Exception (Pd_Minor : in Interfaces.C.Unsigned_Long ;
                                            Pd_Status : in Interfaces.C.Int) ;
   pragma Export (Cpp,
                  C_Raise_Ada_MARSHAL_Exception,
                  "Raise_Ada_MARSHAL_Exception__FUlQ25CORBA16CompletionStatus");

   procedure C_Raise_Ada_INITIALIZATION_FAILURE_Exception (Pd_Minor : in Interfaces.C.Unsigned_Long ;
                                                           Pd_Status : in Interfaces.C.Int) ;
   pragma Export (Cpp,
                  C_Raise_Ada_INITIALIZATION_FAILURE_Exception,
                  "Raise_Ada_INITIALIZE_Exception__FUlQ25CORBA16CompletionStatus");

   procedure C_Raise_Ada_NO_IMPLEMENT_Exception (Pd_Minor : in Interfaces.C.Unsigned_Long ;
                                                 Pd_Status : in Interfaces.C.Int) ;
   pragma Export (Cpp,
                  C_Raise_Ada_NO_IMPLEMENT_Exception,
                  "Raise_Ada_NO_IMPLEMENT_Exception__FUlQ25CORBA16CompletionStatus");

   procedure C_Raise_Ada_BAD_TYPECODE_Exception (Pd_Minor : in Interfaces.C.Unsigned_Long ;
                                                 Pd_Status : in Interfaces.C.Int) ;
   pragma Export (Cpp,
                  C_Raise_Ada_BAD_TYPECODE_Exception,
                  "Raise_Ada_BAD_TYPECODE_Exception__FUlQ25CORBA16CompletionStatus");

   procedure C_Raise_Ada_BAD_OPERATION_Exception (Pd_Minor : in Interfaces.C.Unsigned_Long ;
                                                  Pd_Status : in Interfaces.C.Int) ;
   pragma Export (Cpp,
                  C_Raise_Ada_BAD_OPERATION_Exception,
                  "Raise_Ada_BAD_OPERATION_Exception__FUlQ25CORBA16CompletionStatus");

   procedure C_Raise_Ada_NO_RESOURCES_Exception (Pd_Minor : in Interfaces.C.Unsigned_Long ;
                                                 Pd_Status : in Interfaces.C.Int) ;
   pragma Export (Cpp,
                  C_Raise_Ada_NO_RESOURCES_Exception,
                  "Raise_Ada_NO_RESOURCES_Exception__FUlQ25CORBA16CompletionStatus");

   procedure C_Raise_Ada_NO_RESPONSE_Exception (Pd_Minor : in Interfaces.C.Unsigned_Long ;
                                                Pd_Status : in Interfaces.C.Int) ;
   pragma Export (Cpp,
                  C_Raise_Ada_NO_RESPONSE_Exception,
                  "Raise_Ada_NO_RESPONSE_Exception__FUlQ25CORBA16CompletionStatus");

   procedure C_Raise_Ada_PERSIST_STORE_Exception (Pd_Minor : in Interfaces.C.Unsigned_Long ;
                                                  Pd_Status : in Interfaces.C.Int) ;
   pragma Export (Cpp,
                  C_Raise_Ada_PERSIST_STORE_Exception,
                  "Raise_Ada_PERSIST_STORE_Exception__FUlQ25CORBA16CompletionStatus");

   procedure C_Raise_Ada_BAD_INV_ORDER_Exception (Pd_Minor : in Interfaces.C.Unsigned_Long ;
                                                  Pd_Status : in Interfaces.C.Int) ;
   pragma Export (Cpp,
                  C_Raise_Ada_BAD_INV_ORDER_Exception,
                  "Raise_Ada_BAD_INV_ORDER_Exception__FUlQ25CORBA16CompletionStatus");

   procedure C_Raise_Ada_TRANSIENT_Exception (Pd_Minor : in Interfaces.C.Unsigned_Long ;
                                              Pd_Status : in Interfaces.C.Int) ;
   pragma Export (Cpp,
                  C_Raise_Ada_TRANSIENT_Exception,
                  "Raise_Ada_TRANSIENT_Exception__FUlQ25CORBA16CompletionStatus");

   procedure C_Raise_Ada_FREE_MEM_Exception (Pd_Minor : in Interfaces.C.Unsigned_Long ;
                                             Pd_Status : in Interfaces.C.Int) ;
   pragma Export (Cpp,
                  C_Raise_Ada_FREE_MEM_Exception,
                  "Raise_Ada_FREE_MEM_Exception__FUlQ25CORBA16CompletionStatus");

   procedure C_Raise_Ada_INV_IDENT_Exception (Pd_Minor : in Interfaces.C.Unsigned_Long ;
                                              Pd_Status : in Interfaces.C.Int) ;
   pragma Export (Cpp,
                  C_Raise_Ada_INV_IDENT_Exception,
                  "Raise_Ada_INV_IDENT_Exception__FUlQ25CORBA16CompletionStatus");

   procedure C_Raise_Ada_INV_FLAG_Exception (Pd_Minor : in Interfaces.C.Unsigned_Long ;
                                             Pd_Status : in Interfaces.C.Int) ;
   pragma Export (Cpp,
                  C_Raise_Ada_INV_FLAG_Exception,
                  "Raise_Ada_INV_FLAG_Exception__FUlQ25CORBA16CompletionStatus");

   procedure C_Raise_Ada_INTF_REPOS_Exception (Pd_Minor : in Interfaces.C.Unsigned_Long ;
                                               Pd_Status : in Interfaces.C.Int) ;
   pragma Export (Cpp,
                  C_Raise_Ada_INTF_REPOS_Exception,
                  "Raise_Ada_INTF_REPOS_Exception__FUlQ25CORBA16CompletionStatus");

   procedure C_Raise_Ada_BAD_CONTEXT_Exception (Pd_Minor : in Interfaces.C.Unsigned_Long ;
                                                Pd_Status : in Interfaces.C.Int) ;
   pragma Export (Cpp,
                  C_Raise_Ada_BAD_CONTEXT_Exception,
                  "Raise_Ada_BAD_CONTEXT_Exception__FUlQ25CORBA16CompletionStatus");

   procedure C_Raise_Ada_OBJ_ADAPTER_Exception (Pd_Minor : in Interfaces.C.Unsigned_Long ;
                                                Pd_Status : in Interfaces.C.Int) ;
   pragma Export (Cpp,
                  C_Raise_Ada_OBJ_ADAPTER_Exception,
                  "Raise_Ada_OBJ_ADAPTER_Exception__FUlQ25CORBA16CompletionStatus");

   procedure C_Raise_Ada_DATA_CONVERSION_Exception (Pd_Minor : in Interfaces.C.Unsigned_Long ;
                                                    Pd_Status : in Interfaces.C.Int) ;
   pragma Export (Cpp,
                  C_Raise_Ada_DATA_CONVERSION_Exception,
                  "Raise_Ada_DATA_CONVERSION_Exception__FUlQ25CORBA16CompletionStatus");

   procedure C_Raise_Ada_TRANSACTION_REQUIRED_Exception (Pd_Minor : in Interfaces.C.Unsigned_Long ;
                                                         Pd_Status : in Interfaces.C.Int) ;
   pragma Export (Cpp,
                  C_Raise_Ada_TRANSACTION_REQUIRED_Exception,
                  "Raise_Ada_TRANSACTION_REQUIRED_Exception__FUlQ25CORBA16CompletionStatus");

   procedure C_Raise_Ada_TRANSACTION_ROLLEDBACK_Exception (Pd_Minor : in Interfaces.C.Unsigned_Long ;
                                                           Pd_Status : in Interfaces.C.Int) ;
   pragma Export (Cpp,
                  C_Raise_Ada_TRANSACTION_ROLLEDBACK_Exception,
                  "Raise_Ada_TRANSACTION_ROLLEDBACK_Exception__FUlQ25CORBA16CompletionStatus");

   procedure C_Raise_Ada_INVALID_TRANSACTION_Exception (Pd_Minor : in Interfaces.C.Unsigned_Long ;
                                                        Pd_Status : in Interfaces.C.Int) ;
   pragma Export (Cpp,
                  C_Raise_Ada_INVALID_TRANSACTION_Exception,
                  "Raise_Ada_INVALID_TRANSACTION_Exception__FUlQ25CORBA16CompletionStatus");

   procedure C_Raise_Ada_WRONG_TRANSACTION_Exception (Pd_Minor : in Interfaces.C.Unsigned_Long ;
                                                      Pd_Status : in Interfaces.C.Int) ;
   pragma Export (Cpp,
                  C_Raise_Ada_WRONG_TRANSACTION_Exception,
                  "Raise_Ada_WRONG_TRANSACTION_Exception__FUlQ25CORBA16CompletionStatus");

   procedure C_Raise_Ada_Fatal_Exception (Pd_Minor : in Interfaces.C.Unsigned_Long ;
                                          Pd_Status : in Interfaces.C.Int) ;
   pragma Export (Cpp,
                  C_Raise_Ada_Fatal_Exception,
                  "Raise_Ada_Fatal_Exception__FUlQ25CORBA16CompletionStatus");

end Exceptions ;
