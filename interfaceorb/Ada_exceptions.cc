#include "Ada_exceptions.hh"
#include <omnithread.h>

omni_mutex * occurrence_table_mutex = new omni_mutex ();

void Lock_Occurrence_Table ()
{
  if (omniORB::traceLevel > 5) cerr << "lock occurrence table" << endl;

  occurrence_table_mutex->lock ();
}

void Unlock_Occurrence_Table ()
{
  if (omniORB::traceLevel > 5) cerr << "unlock occurrence table" << endl;

  occurrence_table_mutex->unlock ();
}

/////////////////////////////////
// Handling of Fatal exception //
/////////////////////////////////

void Raise_Corba_Exception (omniORB::fatalException e)
{
  Raise_Ada_Fatal_Exception (e.file(),e.line(),e.errmsg());
}


void Raise_Corba_Exception (CORBA::UNKNOWN e)
{
  CORBA::ULong pd_minor = e.minor () ;
  CORBA::CompletionStatus pd_status = e.completed () ;
  Raise_Ada_UNKNOWN_Exception (pd_minor, pd_status) ;
};


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



