
//  This file contains wrapper functions around functions defined in
//  CORBA.h They are here to handle C++ exceptions which could make
//  the Ada program halt.

#include "Ada_exceptions.hh"

//---------------//
// impl_shutdown //
//---------------//

void impl_shutdown (CORBA::BOA *b)
{
ADABROKER_TRY
  b->impl_shutdown ();
ADABROKER_CATCH
}

//---------//
// destroy //
//---------//

void destroy (CORBA::BOA *b)
{
ADABROKER_TRY
  b->destroy ();
ADABROKER_CATCH
}

//---------------//
// impl_is_ready //
//---------------//

void impl_is_ready (CORBA::BOA *b,
		    CORBA::ImplementationDef_ptr p,
		    CORBA::Boolean NonBlocking=0)
{
ADABROKER_TRY
  b->impl_is_ready (p,NonBlocking);
ADABROKER_CATCH
}
