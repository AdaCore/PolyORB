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

/////////////////////////////////
// Handling of Fatal exception //
/////////////////////////////////

void Raise_Corba_Exception (omniORB::fatalException e)
{
  Raise_Ada_FatalException (e.file(),e.line(),e.errmsg());
}


///////////////////////////////////
// Handling of system exceptions //
///////////////////////////////////

void Raise_Corba_Exception (CORBA::SystemException ex)
{
  CORBA::UNKNOWN *e1 = dynamic_cast<CORBA::UNKNOWN*> (&ex);
  if (e1 != NULL) { Raise_Corba_Exception (*e1); }
  CORBA::BAD_PARAM *e2 = dynamic_cast<CORBA::BAD_PARAM*> (&ex);
  if (e2 != NULL) { Raise_Corba_Exception (*e2); }
  CORBA::NO_MEMORY *e3 = dynamic_cast<CORBA::NO_MEMORY*> (&ex);
  if (e3 != NULL) { Raise_Corba_Exception (*e3); }
  CORBA::IMP_LIMIT *e4 = dynamic_cast<CORBA::IMP_LIMIT*> (&ex);
  if (e4 != NULL) { Raise_Corba_Exception (*e4); }
  CORBA::COMM_FAILURE *e5 = dynamic_cast<CORBA::COMM_FAILURE*> (&ex);
  if (e5 != NULL) { Raise_Corba_Exception (*e5); }
  CORBA::INV_OBJREF *e6 = dynamic_cast<CORBA::INV_OBJREF*> (&ex);
  if (e6 != NULL) { Raise_Corba_Exception (*e6); }
  CORBA::OBJECT_NOT_EXIST *e7 = dynamic_cast<CORBA::OBJECT_NOT_EXIST*> (&ex);
  if (e7 != NULL) { Raise_Corba_Exception (*e7); }
  CORBA::NO_PERMISSION *e8 = dynamic_cast<CORBA::NO_PERMISSION*> (&ex);
  if (e8 != NULL) { Raise_Corba_Exception (*e8); }
  CORBA::INTERNAL *e9 = dynamic_cast<CORBA::INTERNAL*> (&ex);
  if (e9 != NULL) { Raise_Corba_Exception (*e9); }
  CORBA::MARSHAL *e10 = dynamic_cast<CORBA::MARSHAL*> (&ex);
  if (e10 != NULL) { Raise_Corba_Exception (*e10); }
  CORBA::INITIALIZE *e11 = dynamic_cast<CORBA::INITIALIZE*> (&ex);
  if (e11 != NULL) { Raise_Corba_Exception (*e11); }
  CORBA::NO_IMPLEMENT *e12 = dynamic_cast<CORBA::NO_IMPLEMENT*> (&ex);
  if (e12 != NULL) { Raise_Corba_Exception (*e12); }
  CORBA::BAD_TYPECODE *e13 = dynamic_cast<CORBA::BAD_TYPECODE*> (&ex);
  if (e13 != NULL) { Raise_Corba_Exception (*e13); }
  CORBA::BAD_OPERATION *e14 = dynamic_cast<CORBA::BAD_OPERATION*> (&ex);
  if (e14 != NULL) { Raise_Corba_Exception (*e14); }
  CORBA::NO_RESOURCES *e15 = dynamic_cast<CORBA::NO_RESOURCES*> (&ex);
  if (e15 != NULL) { Raise_Corba_Exception (*e15); }
  CORBA::NO_RESPONSE *e16 = dynamic_cast<CORBA::NO_RESPONSE*> (&ex);
  if (e16 != NULL) { Raise_Corba_Exception (*e16); }
  CORBA::PERSIST_STORE *e17 = dynamic_cast<CORBA::PERSIST_STORE*> (&ex);
  if (e17 != NULL) { Raise_Corba_Exception (*e17); }
  CORBA::BAD_INV_ORDER *e18 = dynamic_cast<CORBA::BAD_INV_ORDER*> (&ex);
  if (e18 != NULL) { Raise_Corba_Exception (*e18); }
  CORBA::TRANSIENT *e19 = dynamic_cast<CORBA::TRANSIENT*> (&ex);
  if (e19 != NULL) { Raise_Corba_Exception (*e19); }
  CORBA::FREE_MEM *e20 = dynamic_cast<CORBA::FREE_MEM*> (&ex);
  if (e20 != NULL) { Raise_Corba_Exception (*e20); }
  CORBA::INV_IDENT *e21 = dynamic_cast<CORBA::INV_IDENT*> (&ex);
  if (e21 != NULL) { Raise_Corba_Exception (*e21); }
  CORBA::INV_FLAG *e22 = dynamic_cast<CORBA::INV_FLAG*> (&ex);
  if (e22 != NULL) { Raise_Corba_Exception (*e22); }
  CORBA::INTF_REPOS *e23 = dynamic_cast<CORBA::INTF_REPOS*> (&ex);
  if (e23 != NULL) { Raise_Corba_Exception (*e23); }
  CORBA::BAD_CONTEXT *e24 = dynamic_cast<CORBA::BAD_CONTEXT*> (&ex);
  if (e24 != NULL) { Raise_Corba_Exception (*e24); }
  CORBA::OBJ_ADAPTER *e25 = dynamic_cast<CORBA::OBJ_ADAPTER*> (&ex);
  if (e25 != NULL) { Raise_Corba_Exception (*e25); }
  CORBA::DATA_CONVERSION *e26 = dynamic_cast<CORBA::DATA_CONVERSION*> (&ex);
  if (e26 != NULL) { Raise_Corba_Exception (*e26); }
  CORBA::TRANSACTION_REQUIRED *e27 = dynamic_cast<CORBA::TRANSACTION_REQUIRED*> (&ex);
  if (e27 != NULL) { Raise_Corba_Exception (*e27); }
  CORBA::TRANSACTION_ROLLEDBACK *e28 = dynamic_cast<CORBA::TRANSACTION_ROLLEDBACK*> (&ex);
  if (e28 != NULL) { Raise_Corba_Exception (*e28); }
  CORBA::INVALID_TRANSACTION *e29 = dynamic_cast<CORBA::INVALID_TRANSACTION*> (&ex);
  if (e29 != NULL) { Raise_Corba_Exception (*e29); }
  CORBA::WRONG_TRANSACTION *e30 = dynamic_cast<CORBA::WRONG_TRANSACTION*> (&ex);
  if (e30 != NULL) { Raise_Corba_Exception (*e30); }
  Raise_Ada_FatalException ("Ada_exceptions.cc",
			    127,
			    "An unknown C system exception was catched.\nI can not raise it in Ada.");  
}


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

void Raise_Corba_Exception (CORBA::MARSHAL e)
{
  CORBA::ULong pd_minor = e.minor () ;
  CORBA::CompletionStatus pd_status = e.completed () ;
  Raise_Ada_MARSHAL_Exception (pd_minor, pd_status) ;
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



