#ifndef __ADA_CORBA_BOA_H__
#define __ADA_CORBA_BOA_H__

#include <omniORB/CORBA.h>

void impl_shutdown (CORBA::BOA *b);
//wrapper around void impl_shutdown();
// (see CORBA.h L 1972)

void destroy (CORBA::BOA *b);
// wrapper around void impl_shutdown();
// (see CORBA.h L 1988)

void impl_is_ready (CORBA::BOA *b,
		    CORBA::ImplementationDef_ptr p = 0,
		    CORBA::Boolean NonBlocking = 0) ;
// wrapper around
//    void impl_is_ready(ImplementationDef_ptr=0, Boolean NonBlocking=0);
// (see CORBA.h L 1967)

#endif
