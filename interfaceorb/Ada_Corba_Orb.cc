
//  This file contains wrapper functions around functions defined in
//  CORBA.h They are here to handle C++ exceptions which could make
//  the Ada program halt.

#include <omniORB2/CORBA.h>
#include "Ada_exceptions.hh"

//--------------//      
// Ada_ORB_init //
//--------------//

CORBA::ORB_ptr
Ada_ORB_init(int argc, char **argv,const char *orb_identifier)
{
ADABROKER_TRY
#ifdef DEBUG
  omniORB::traceLevel = 100;
#endif
  return CORBA::ORB_init(argc, argv, orb_identifier) ;
ADABROKER_CATCH
  // never reach here just a default return for dummy compilers.
  CORBA::ORB_ptr default_return = NULL;
  return  default_return; 
}

//--------------//
// Ada_BOA_init //
//--------------//

CORBA::BOA_ptr
Ada_BOA_init(CORBA::ORB_ptr orb,
	     int argc,
	     char **argv,
	     const char *boa_identifier)
{
ADABROKER_TRY
  return orb->BOA_init(argc, argv, boa_identifier) ;
ADABROKER_CATCH
  // never reach here just a default return for dummy compilers.
  CORBA::BOA_ptr default_return = NULL;
  return  default_return; 
}
