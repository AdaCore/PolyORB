#ifndef __ADA_CORBA_ORB_H__
#define __ADA_CORBA_ORB_H__

#include <omniORB2/CORBA.h>

CORBA::ORB_ptr
Ada_ORB_init(int           argc,
             char       ** argv,
             const char *  orb_identifier,
             int           traceLevel);
// this function is a wrapper around CORBA::ORB_init
//in corbaOrb.cc L170
// it is called by Ada (Corba.Orb.adb)
// and is here to handle C++ exceptions
// it takes a int and not a int& as first paramteter
// because in Ada, it is an "in" parameter


CORBA::BOA_ptr
Ada_BOA_init(CORBA::ORB_ptr  orb,
	     int             argc,
	     char         ** argv,
	     const char    * boa_identifier)
// this function is a wrapper around CORBA::BOA_init
//in corbaBoa.cc L180
// it is called by Ada (Corba.Orb.adb)
// and is here to handle C++ exceptions
// it takes a int and not a int& as first paramteter
// because in Ada, it is an "in" parameter

#endif
