///////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////
////                                                               ////
////                         AdaBroker                             ////
////                                                               ////
////                                                               ////
////   Copyright (C) 1999 ENST                                     ////
////                                                               ////
////   This file is part of the AdaBroker library                  ////
////                                                               ////
////   The AdaBroker library is free software; you can             ////
////   redistribute it and/or modify it under the terms of the     ////
////   GNU Library General Public License as published by the      ////
////   Free Software Foundation; either version 2 of the License,  ////
////   or (at your option) any later version.                      ////
////                                                               ////
////   This library is distributed in the hope that it will be     ////
////   useful, but WITHOUT ANY WARRANTY; without even the implied  ////
////   warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR     ////
////   PURPOSE.  See the GNU Library General Public License for    ////
////   more details.                                               ////
////                                                               ////
////   You should have received a copy of the GNU Library General  ////
////   Public License along with this library; if not, write to    ////
////   the Free Software Foundation, Inc., 59 Temple Place -       ////
////   Suite 330, Boston, MA 02111-1307, USA                       ////
////                                                               ////
////                                                               ////
////                                                               ////
////   Description                                                 ////
////   -----------                                                 ////
////     This package deals with the raising of C exceptions in    ////
////   Ada and ada ones in C.                                      ////
////     It is both a C and a Ada class (see exceptions.ads)       ////
////   and provides 2 mains methods : raise_C_Exception and        ////
////   raise_Ada_Exception. The first one is called by Ada code    ////
////   and implemented in C. The second is called by C code and    ////
////   implemented in Ada. Both translate exceptions in the other  ////
////   language.                                                   ////
////                                                               ////
////                                                               ////
////   authors : Sebastien Ponce, Fabien Azavant                   ////
////   date    : 02/28/99                                          ////
////                                                               ////
///////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////


#include "Ada_exceptions.hh"

///////////////////////////////////
// Handling of UNKNOWN exception //
///////////////////////////////////

// Raise_Corba_Exception
//----------------------
void Raise_Corba_Exception (CORBA::UNKNOWN e)
{
  CORBA::ULong pd_minor = e.minor () ;
  CORBA::CompletionStatus pd_status = e.completed () ;
  Raise_Ada_UNKNOWN_Exception (pd_minor, pd_status) ;
};



///////////////////////////////////////////////////////
// And now the same methods for the other exceptions //
///////////////////////////////////////////////////////

void Raise_Corba_Exception (CORBA::BAD_PARAM e)
{
  CORBA::ULong pd_minor = e.minor () ;
  CORBA::CompletionStatus pd_status = e.completed () ;
  Raise_Ada_BAD_PARAM_Exception (pd_minor, pd_status) ;
};

void Raise_Corba_Exception (CORBA::NO_MEMORY e)
{
  CORBA::ULong pd_minor = e.minor () ;
  CORBA::CompletionStatus pd_status = e.completed () ;
  Raise_Ada_NO_MEMORY_Exception (pd_minor, pd_status) ;
};

void Raise_Corba_Exception (CORBA::IMP_LIMIT e)
{
  CORBA::ULong pd_minor = e.minor () ;
  CORBA::CompletionStatus pd_status = e.completed () ;
  Raise_Ada_IMP_LIMIT_Exception (pd_minor, pd_status) ;
};

void Raise_Corba_Exception (CORBA::COMM_FAILURE e)
{
  CORBA::ULong pd_minor = e.minor () ;
  CORBA::CompletionStatus pd_status = e.completed () ;
  Raise_Ada_COMM_FAILURE_Exception (pd_minor, pd_status) ;
};

void Raise_Corba_Exception (CORBA::INV_OBJREF e)
{
  CORBA::ULong pd_minor = e.minor () ;
  CORBA::CompletionStatus pd_status = e.completed () ;
  Raise_Ada_INV_OBJREF_Exception (pd_minor, pd_status) ;
};

void Raise_Corba_Exception (CORBA::OBJECT_NOT_EXIST e)
{
  CORBA::ULong pd_minor = e.minor () ;
  CORBA::CompletionStatus pd_status = e.completed () ;
  Raise_Ada_OBJECT_NOT_EXIST_Exception (pd_minor, pd_status) ;
};

void Raise_Corba_Exception (CORBA::NO_PERMISSION e)
{
  CORBA::ULong pd_minor = e.minor () ;
  CORBA::CompletionStatus pd_status = e.completed () ;
  Raise_Ada_NO_PERMISSION_Exception (pd_minor, pd_status) ;
};

void Raise_Corba_Exception (CORBA::INTERNAL e)
{
  CORBA::ULong pd_minor = e.minor () ;
  CORBA::CompletionStatus pd_status = e.completed () ;
  Raise_Ada_INTERNAL_Exception (pd_minor, pd_status) ;
};

void Raise_Corba_Exception (CORBA::INITIALIZE e)
{
  CORBA::ULong pd_minor = e.minor () ;
  CORBA::CompletionStatus pd_status = e.completed () ;
  Raise_Ada_INITIALIZE_Exception (pd_minor, pd_status) ;
};

void Raise_Corba_Exception (CORBA::NO_IMPLEMENT e)
{
  CORBA::ULong pd_minor = e.minor () ;
  CORBA::CompletionStatus pd_status = e.completed () ;
  Raise_Ada_NO_IMPLEMENT_Exception (pd_minor, pd_status) ;
};

void Raise_Corba_Exception (CORBA::BAD_TYPECODE e)
{
  CORBA::ULong pd_minor = e.minor () ;
  CORBA::CompletionStatus pd_status = e.completed () ;
  Raise_Ada_BAD_TYPECODE_Exception (pd_minor, pd_status) ;
};

void Raise_Corba_Exception (CORBA::BAD_OPERATION e)
{
  CORBA::ULong pd_minor = e.minor () ;
  CORBA::CompletionStatus pd_status = e.completed () ;
  Raise_Ada_BAD_OPERATION_Exception (pd_minor, pd_status) ;
};

void Raise_Corba_Exception (CORBA::NO_RESOURCES e)
{
  CORBA::ULong pd_minor = e.minor () ;
  CORBA::CompletionStatus pd_status = e.completed () ;
  Raise_Ada_NO_RESOURCES_Exception (pd_minor, pd_status) ;
};

void Raise_Corba_Exception (CORBA::NO_RESPONSE e)
{
  CORBA::ULong pd_minor = e.minor () ;
  CORBA::CompletionStatus pd_status = e.completed () ;
  Raise_Ada_NO_RESPONSE_Exception (pd_minor, pd_status) ;
};

void Raise_Corba_Exception (CORBA::PERSIST_STORE e)
{
  CORBA::ULong pd_minor = e.minor () ;
  CORBA::CompletionStatus pd_status = e.completed () ;
  Raise_Ada_PERSIST_STORE_Exception (pd_minor, pd_status) ;
};

void Raise_Corba_Exception (CORBA::BAD_INV_ORDER e)
{
  CORBA::ULong pd_minor = e.minor () ;
  CORBA::CompletionStatus pd_status = e.completed () ;
  Raise_Ada_BAD_INV_ORDER_Exception (pd_minor, pd_status) ;
};

void Raise_Corba_Exception (CORBA::TRANSIENT e)
{
  CORBA::ULong pd_minor = e.minor () ;
  CORBA::CompletionStatus pd_status = e.completed () ;
  Raise_Ada_TRANSIENT_Exception (pd_minor, pd_status) ;
};

void Raise_Corba_Exception (CORBA::FREE_MEM e)
{
  CORBA::ULong pd_minor = e.minor () ;
  CORBA::CompletionStatus pd_status = e.completed () ;
  Raise_Ada_FREE_MEM_Exception (pd_minor, pd_status) ;
};

void Raise_Corba_Exception (CORBA::INV_IDENT e)
{
  CORBA::ULong pd_minor = e.minor () ;
  CORBA::CompletionStatus pd_status = e.completed () ;
  Raise_Ada_INV_IDENT_Exception (pd_minor, pd_status) ;
};

void Raise_Corba_Exception (CORBA::INV_FLAG e)
{
  CORBA::ULong pd_minor = e.minor () ;
  CORBA::CompletionStatus pd_status = e.completed () ;
  Raise_Ada_INV_FLAG_Exception (pd_minor, pd_status) ;
};

void Raise_Corba_Exception (CORBA::INTF_REPOS e)
{
  CORBA::ULong pd_minor = e.minor () ;
  CORBA::CompletionStatus pd_status = e.completed () ;
  Raise_Ada_INTF_REPOS_Exception (pd_minor, pd_status) ;
};

void Raise_Corba_Exception (CORBA::BAD_CONTEXT e)
{
  CORBA::ULong pd_minor = e.minor () ;
  CORBA::CompletionStatus pd_status = e.completed () ;
  Raise_Ada_BAD_CONTEXT_Exception (pd_minor, pd_status) ;
};

void Raise_Corba_Exception (CORBA::OBJ_ADAPTER e)
{
  CORBA::ULong pd_minor = e.minor () ;
  CORBA::CompletionStatus pd_status = e.completed () ;
  Raise_Ada_OBJ_ADAPTER_Exception (pd_minor, pd_status) ;
};

void Raise_Corba_Exception (CORBA::DATA_CONVERSION e)
{
  CORBA::ULong pd_minor = e.minor () ;
  CORBA::CompletionStatus pd_status = e.completed () ;
  Raise_Ada_DATA_CONVERSION_Exception (pd_minor, pd_status) ;
};

void Raise_Corba_Exception (CORBA::TRANSACTION_REQUIRED e)
{
  CORBA::ULong pd_minor = e.minor () ;
  CORBA::CompletionStatus pd_status = e.completed () ;
  Raise_Ada_TRANSACTION_REQUIRED_Exception (pd_minor, pd_status) ;
};

void Raise_Corba_Exception (CORBA::TRANSACTION_ROLLEDBACK e)
{
  CORBA::ULong pd_minor = e.minor () ;
  CORBA::CompletionStatus pd_status = e.completed () ;
  Raise_Ada_TRANSACTION_ROLLEDBACK_Exception (pd_minor, pd_status) ;
};

void Raise_Corba_Exception (CORBA::INVALID_TRANSACTION e)
{
  CORBA::ULong pd_minor = e.minor () ;
  CORBA::CompletionStatus pd_status = e.completed () ;
  Raise_Ada_INVALID_TRANSACTION_Exception (pd_minor, pd_status) ;
};

void Raise_Corba_Exception (CORBA::WRONG_TRANSACTION e)
{
  CORBA::ULong pd_minor = e.minor () ;
  CORBA::CompletionStatus pd_status = e.completed () ;
  Raise_Ada_WRONG_TRANSACTION_Exception (pd_minor, pd_status) ;
};

