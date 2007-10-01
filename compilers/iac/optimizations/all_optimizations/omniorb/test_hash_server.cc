#include <iostream>
#include <stdlib.h>
#include "test_hash.hh"
#include <stdio.h>

using namespace std;
using namespace CORBA;

// implementation of the tests
#include "test_hash_imp.cc"

int main(int argc, char** argv)
{
  
  // Creating a simple server
  // See omniorb documentation for explanations
  try {
    CORBA::ORB_var orb = CORBA::ORB_init(argc, argv);

    CORBA::Object_var obj = orb->resolve_initial_references("RootPOA");

    PortableServer::POA_var poa = PortableServer::POA::_narrow(obj);

    test_hash_i* myallt = new test_hash_i();
    
    PortableServer::ObjectId_var myalltid = poa->activate_object(myallt);

    obj = myallt->_this();

    CORBA::String_var sior(orb->object_to_string(obj));
    cerr << "'" << (char*) sior << "'" << endl;
    
    myallt->_remove_ref();

    PortableServer::POAManager_var pman = poa->the_POAManager();
    pman->activate();

    orb->run();
  }

  catch(...)
    {
      cerr << "fatal error : exception reached by ther server" << endl;
    }

  return EXIT_SUCCESS;
}
