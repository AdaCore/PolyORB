// eg2_impl.cc - This is the source code of example 2 used in Chapter 2
//               "The Basics" of the omniORB2 user guide.
//
//               This is the object implementation.
//
// Usage: eg2_impl
//
//        On startup, the object reference is printed to cerr as a
//        stringified IOR. This string should be used as the argument to 
//        eg2_clt.
//
#include <iostream.h>
#include "omnithread.h"
#include "echo.hh"

#include "echo_i.cc"

int
main(int argc, char **argv)
{
  CORBA::ORB_ptr orb = CORBA::ORB_init(argc,argv,"omniORB2");
  CORBA::BOA_ptr boa = orb->BOA_init(argc,argv,"omniORB2_BOA");

  Echo_i *myobj = new Echo_i();
  myobj->_obj_is_ready(boa);

  {
    Echo_var myobjRef = myobj->_this();
    CORBA::String_var p = orb->object_to_string(myobjRef);
    cerr << "'" << (char*)p << "'" << endl;
  }

  boa->impl_is_ready();
  // Tell the BOA we are ready. The BOA's default behaviour is to block
  // on this call indefinitely.

  // Call boa->impl_shutdown() from another thread would unblock the
  // main thread from impl_is_ready().
  //
  // To properly shutdown the BOA and the ORB, add the following calls
  // after impl_is_ready() returns.
  //
  // boa->destroy();
  // orb->NP_destroy();
 
  return 0;
}
