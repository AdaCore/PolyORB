//--------------------------------------------------------------------------//
//                                                                          //
//                          ADABROKER COMPONENTS                            //
//                                                                          //
//                            A D A B R O K E R                             //
//                                                                          //
//                            $Revision: 1.12 $
//                                                                          //
//         Copyright (C) 1999-2000 ENST Paris University, France.           //
//                                                                          //
// AdaBroker is free software; you  can  redistribute  it and/or modify it  //
// under terms of the  GNU General Public License as published by the  Free //
// Software Foundation;  either version 2,  or (at your option)  any  later //
// version. AdaBroker  is distributed  in the hope that it will be  useful, //
// but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- //
// TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public //
// License  for more details.  You should have received  a copy of the GNU  //
// General Public License distributed with AdaBroker; see file COPYING. If  //
// not, write to the Free Software Foundation, 59 Temple Place - Suite 330, //
// Boston, MA 02111-1307, USA.                                              //
//                                                                          //
// As a special exception,  if other files  instantiate  generics from this //
// unit, or you link  this unit with other files  to produce an executable, //
// this  unit  does not  by itself cause  the resulting  executable  to  be //
// covered  by the  GNU  General  Public  License.  This exception does not //
// however invalidate  any other reasons why  the executable file  might be //
// covered by the  GNU Public License.                                      //
//                                                                          //
//             AdaBroker is maintained by ENST Paris University.            //
//                     (email: broker@inf.enst.fr)                          //
//                                                                          //
//--------------------------------------------------------------------------//
#ifndef __ADA_EXCEPTIONS_H__
#define __ADA_EXCEPTIONS_H__

#include <omniORB2/CORBA.h>

extern void Lock_Occurrence_Table ();
extern void Unlock_Occurrence_Table ();

///////////////////////////////////////////
// handling od corba exception in C code //
///////////////////////////////////////////

#define ADABROKER_TRY try {
#define ADABROKER_CATCH \
} catch (CORBA::UNKNOWN &e) { \
  Raise_Corba_Exception (e); \
} catch (CORBA::BAD_PARAM &e) { \
  Raise_Corba_Exception (e); \
} catch (CORBA::NO_MEMORY &e) { \
  Raise_Corba_Exception (e); \
} catch (CORBA::IMP_LIMIT &e) { \
  Raise_Corba_Exception (e); \
} catch (CORBA::COMM_FAILURE &e) { \
  Raise_Corba_Exception (e); \
} catch (CORBA::INV_OBJREF &e) { \
  Raise_Corba_Exception (e); \
} catch (CORBA::NO_PERMISSION &e) { \
  Raise_Corba_Exception (e); \
} catch (CORBA::INTERNAL &e) { \
  Raise_Corba_Exception (e); \
} catch (CORBA::MARSHAL &e) { \
  Raise_Corba_Exception (e); \
} catch (CORBA::INITIALIZE &e) { \
  Raise_Corba_Exception (e); \
} catch (CORBA::NO_IMPLEMENT &e) { \
  Raise_Corba_Exception (e); \
} catch (CORBA::BAD_TYPECODE &e) { \
  Raise_Corba_Exception (e); \
} catch (CORBA::BAD_OPERATION &e) { \
  Raise_Corba_Exception (e); \
} catch (CORBA::NO_RESOURCES &e) { \
  Raise_Corba_Exception (e); \
} catch (CORBA::NO_RESPONSE &e) { \
  Raise_Corba_Exception (e); \
} catch (CORBA::PERSIST_STORE &e) { \
  Raise_Corba_Exception (e); \
} catch (CORBA::BAD_INV_ORDER &e) { \
  Raise_Corba_Exception (e); \
} catch (CORBA::TRANSIENT &e) { \
  Raise_Corba_Exception (e); \
} catch (CORBA::FREE_MEM &e) { \
  Raise_Corba_Exception (e); \
} catch (CORBA::INITIALIZE &e) { \
  Raise_Corba_Exception (e); \
} catch (CORBA::INV_IDENT &e) { \
  Raise_Corba_Exception (e); \
} catch (CORBA::INV_FLAG &e) { \
  Raise_Corba_Exception (e); \
} catch (CORBA::INTF_REPOS &e) { \
  Raise_Corba_Exception (e); \
} catch (CORBA::BAD_CONTEXT &e) { \
  Raise_Corba_Exception (e); \
} catch (CORBA::OBJ_ADAPTER &e) { \
  Raise_Corba_Exception (e); \
} catch (CORBA::DATA_CONVERSION &e) { \
  Raise_Corba_Exception (e); \
} catch (CORBA::OBJECT_NOT_EXIST &e) { \
  Raise_Corba_Exception (e); \
} catch (CORBA::TRANSACTION_REQUIRED &e) { \
  Raise_Corba_Exception (e); \
} catch (CORBA::TRANSACTION_ROLLEDBACK &e) { \
  Raise_Corba_Exception (e); \
} catch (CORBA::INVALID_TRANSACTION &e) { \
  Raise_Corba_Exception (e); \
} catch (CORBA::WRONG_TRANSACTION &e) { \
  Raise_Corba_Exception (e); \
} catch (...) { \
  Raise_Ada_Fatal_Exception (__FILE__, \
			     __LINE__, \
			     "An unknown C exception was catched.\nI can not raise it in Ada."); \
}


/////////////////////////////////
// Handling of Fatal exception //
/////////////////////////////////

void Raise_Corba_Exception (omniORB::fatalException e);
// This method is called by C code for raising Corba exception
// in Ada code. It uses Raise_Ada_Exception to handle the exceptions

extern void Raise_Ada_Fatal_Exception (const char* file,
                                        int line,
                                       const char* err_msg);
// called by C code (Raise_Corba_Exception to be exact).
// Handles in Ada a Corba exception that was raised in C.
// (see omniORB.h L 471 for more details on fatalexception)


///////////////////////////////////
// Handling of UNKNOWN exception //
///////////////////////////////////

void Raise_Corba_Exception (CORBA::UNKNOWN e);
// This method is called by C code for raising Corba exception
// in Ada code. It uses Raise_Ada_Exception to handle the exceptions

extern void Raise_Ada_UNKNOWN_Exception (CORBA::ULong pd_minor,
                                         CORBA::CompletionStatus pd_status);
// called by C code (Raise_Corba_Exception to be exact).
// Handles in Ada a Corba exception that was raised in C.


///////////////////////////////////////////////////////
// And now the same methods for the other exceptions //
///////////////////////////////////////////////////////

void Raise_Corba_Exception (CORBA::BAD_PARAM e);

extern void Raise_Ada_BAD_PARAM_Exception (CORBA::ULong pd_minor,
                                           CORBA::CompletionStatus pd_status);

void Raise_Corba_Exception (CORBA::NO_MEMORY e);

extern void Raise_Ada_NO_MEMORY_Exception (CORBA::ULong pd_minor,
                                           CORBA::CompletionStatus pd_status);

void Raise_Corba_Exception (CORBA::IMP_LIMIT e);

extern void Raise_Ada_IMP_LIMIT_Exception (CORBA::ULong pd_minor,
                                           CORBA::CompletionStatus pd_status);

void Raise_Corba_Exception (CORBA::COMM_FAILURE e);

extern void Raise_Ada_COMM_FAILURE_Exception (CORBA::ULong pd_minor,
                                              CORBA::CompletionStatus pd_status);

void Raise_Corba_Exception (CORBA::INV_OBJREF e);

extern void Raise_Ada_INV_OBJREF_Exception (CORBA::ULong pd_minor,
                                            CORBA::CompletionStatus pd_status);

void Raise_Corba_Exception (CORBA::OBJECT_NOT_EXIST e);

extern void Raise_Ada_OBJECT_NOT_EXIST_Exception (CORBA::ULong pd_minor,
                                                  CORBA::CompletionStatus pd_status);

void Raise_Corba_Exception (CORBA::NO_PERMISSION e);

extern void Raise_Ada_NO_PERMISSION_Exception (CORBA::ULong pd_minor,
                                               CORBA::CompletionStatus pd_status);

void Raise_Corba_Exception (CORBA::INTERNAL e);

extern void Raise_Ada_INTERNAL_Exception (CORBA::ULong pd_minor,
                                          CORBA::CompletionStatus pd_status);

void Raise_Corba_Exception (CORBA::MARSHAL e);

extern void Raise_Ada_MARSHAL_Exception (CORBA::ULong pd_minor,
                                         CORBA::CompletionStatus pd_status);

void Raise_Corba_Exception (CORBA::INITIALIZE e);

extern void Raise_Ada_INITIALIZE_Exception (CORBA::ULong pd_minor,
                                            CORBA::CompletionStatus pd_status);

void Raise_Corba_Exception (CORBA::NO_IMPLEMENT e);

extern void Raise_Ada_NO_IMPLEMENT_Exception (CORBA::ULong pd_minor,
                                              CORBA::CompletionStatus pd_status);

void Raise_Corba_Exception (CORBA::BAD_TYPECODE e);

extern void Raise_Ada_BAD_TYPECODE_Exception (CORBA::ULong pd_minor,
                                              CORBA::CompletionStatus pd_status);

void Raise_Corba_Exception (CORBA::BAD_OPERATION e);

extern void Raise_Ada_BAD_OPERATION_Exception (CORBA::ULong pd_minor,
                                               CORBA::CompletionStatus pd_status);

void Raise_Corba_Exception (CORBA::NO_RESOURCES e);

extern void Raise_Ada_NO_RESOURCES_Exception (CORBA::ULong pd_minor,
                                              CORBA::CompletionStatus pd_status);

void Raise_Corba_Exception (CORBA::NO_RESPONSE e);

extern void Raise_Ada_NO_RESPONSE_Exception (CORBA::ULong pd_minor,
                                             CORBA::CompletionStatus pd_status);

void Raise_Corba_Exception (CORBA::PERSIST_STORE e);

extern void Raise_Ada_PERSIST_STORE_Exception (CORBA::ULong pd_minor,
                                               CORBA::CompletionStatus pd_status);

void Raise_Corba_Exception (CORBA::BAD_INV_ORDER e);

extern void Raise_Ada_BAD_INV_ORDER_Exception (CORBA::ULong pd_minor,
                                               CORBA::CompletionStatus pd_status);

void Raise_Corba_Exception (CORBA::TRANSIENT e);

extern void Raise_Ada_TRANSIENT_Exception (CORBA::ULong pd_minor,
                                           CORBA::CompletionStatus pd_status);

void Raise_Corba_Exception (CORBA::FREE_MEM e);

extern void Raise_Ada_FREE_MEM_Exception (CORBA::ULong pd_minor,
                                          CORBA::CompletionStatus pd_status);

void Raise_Corba_Exception (CORBA::INV_IDENT e);

extern void Raise_Ada_INV_IDENT_Exception (CORBA::ULong pd_minor,
                                           CORBA::CompletionStatus pd_status);

void Raise_Corba_Exception (CORBA::INV_FLAG e);

extern void Raise_Ada_INV_FLAG_Exception (CORBA::ULong pd_minor,
                                          CORBA::CompletionStatus pd_status);

void Raise_Corba_Exception (CORBA::INTF_REPOS e);

extern void Raise_Ada_INTF_REPOS_Exception (CORBA::ULong pd_minor,
                                            CORBA::CompletionStatus pd_status);

void Raise_Corba_Exception (CORBA::BAD_CONTEXT e);

extern void Raise_Ada_BAD_CONTEXT_Exception (CORBA::ULong pd_minor,
                                             CORBA::CompletionStatus pd_status);

void Raise_Corba_Exception (CORBA::OBJ_ADAPTER e);

extern void Raise_Ada_OBJ_ADAPTER_Exception (CORBA::ULong pd_minor,
                                             CORBA::CompletionStatus pd_status);

void Raise_Corba_Exception (CORBA::DATA_CONVERSION e);

extern void Raise_Ada_DATA_CONVERSION_Exception (CORBA::ULong pd_minor,
                                                 CORBA::CompletionStatus pd_status);

void Raise_Corba_Exception (CORBA::TRANSACTION_REQUIRED e);

extern void Raise_Ada_TRANSACTION_REQUIRED_Exception (CORBA::ULong pd_minor,
                                                      CORBA::CompletionStatus pd_status);

void Raise_Corba_Exception (CORBA::TRANSACTION_ROLLEDBACK e);

extern void Raise_Ada_TRANSACTION_ROLLEDBACK_Exception (CORBA::ULong pd_minor,
                                                        CORBA::CompletionStatus pd_status);

void Raise_Corba_Exception (CORBA::INVALID_TRANSACTION e);

extern void Raise_Ada_INVALID_TRANSACTION_Exception (CORBA::ULong pd_minor,
                                                     CORBA::CompletionStatus pd_status);

void Raise_Corba_Exception (CORBA::WRONG_TRANSACTION e);

extern void Raise_Ada_WRONG_TRANSACTION_Exception (CORBA::ULong pd_minor,
                                                   CORBA::CompletionStatus pd_status);

#endif
