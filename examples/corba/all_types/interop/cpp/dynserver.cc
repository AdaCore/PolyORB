// $Id: //droopi/main/examples/corba/all_types/interop/cpp/dynserver.cc#4 $
// DSI server, implements echoULong method

#include <iostream>
#include <stdlib.h>
#include <stdio.h>

#ifdef __USE_TAO__
#include "tao/DynamicInterface/Dynamic_Implementation.h"
#include "tao/DynamicInterface/Server_Request.h"
#include "tao/PortableServer/PortableServer.h"
#include "tao/corba.h"
#endif

#ifdef __USE_OMNIORB__
#include <omniORB4/CORBA.h>
#endif

using namespace std;
using namespace CORBA;

class MyDynImpl : public PortableServer::DynamicImplementation,
                  public PortableServer::RefCountServantBase {
public:
  void invoke(CORBA::ServerRequest_ptr request);
  virtual char* _primary_interface(const PortableServer::ObjectId&,
                                   PortableServer::POA_ptr);
};

CORBA::ORB_var orb;

void
MyDynImpl::invoke(CORBA::ServerRequest_ptr request)
{
  try {

    if (strcmp ("_is_a", request->operation ()) == 0)
      {
	CORBA::NVList_ptr list;
	orb->create_list (0, list);

	// XXX It seems there is a subtle difference in the way
	// anys are handled. This makes the compiler happy, but 
	// is it correct ?

#ifdef __USE_TAO__
	CORBA::Any type_id (CORBA::_tc_string);
#endif

#ifdef __USE_OMNIORB__
	CORBA::Any type_id;
	type_id.replace(CORBA::_tc_string, 0);
#endif
	
	list->add_value ("type_id", type_id, CORBA::ARG_IN);
	
	request->arguments (list);
	
	CORBA::NamedValue_ptr nv = list->item (0);
	
	const char *arg;
	*(nv->value ()) >>= arg;
	
	CORBA::Boolean type_matches = 0;
	if (strcmp (arg, "IDL:all_types:1.0") == 0
	    || strcmp (arg, "IDL:omg.org/CORBA/Object:1.0") == 0
	    || strcmp (arg, "") == 0)
	  type_matches = 1;
	
	CORBA::Any result;
	result <<= CORBA::Any::from_boolean (type_matches);
	
	request->set_result (result);
	
	return;
      }
    
    else if ( strcmp("echoULong", request->operation()) == 0)
      {
	CORBA::NVList_ptr args;
	orb->create_list(0, args);

	//  XXX See comment above

#ifdef __USE_TAO__
	CORBA::Any a (CORBA::_tc_ulong);
#endif

#ifdef __USE_OMNIORB__
	CORBA::Any a;
	a.replace(CORBA::_tc_ulong, 0);
#endif

	args->add_value("", a, CORBA::ARG_IN);
	request->arguments(args);
	CORBA::ULong x;
	*(args->item(0)->value()) >>= x;
	CORBA::Any* result = new CORBA::Any();
	*result <<= x;
	request->set_result(*result);
	delete result;
      }
    else
      {
	std::cout << "bad operation :" << request->operation() << endl;

	throw CORBA::BAD_OPERATION(0, CORBA::COMPLETED_NO);
      }
  }
  catch(CORBA::SystemException& ex){
    std::cout << "echo_dsiimpl: MyDynImpl::invoke - caught an system exception."
	      << endl;
    CORBA::Any a;
    a <<= ex;
    request->set_exception(a);
  }
  catch(...){
    std::cout << "echo_dsiimpl: MyDynImpl::invoke - caught an unknown exception."
	      << endl;
    CORBA::Any a;
    a <<= CORBA::UNKNOWN(0, CORBA::COMPLETED_NO);
    request->set_exception(a);
  }
}

char*
MyDynImpl::_primary_interface(const PortableServer::ObjectId&,
                              PortableServer::POA_ptr)
{
  return CORBA::string_dup("IDL:all_types:1.0");
}

int main(int argc, char** argv)
{
  
  // Creating a simple server

  try {
    orb = CORBA::ORB_init(argc, argv);
    std::cerr << "@@1" << std::endl;

    CORBA::Object_var obj = orb->resolve_initial_references("RootPOA");
    std::cerr << "@@2" << std::endl;

    PortableServer::POA_var poa = PortableServer::POA::_narrow(obj.in ());
    std::cerr << "@@3" << std::endl;

    MyDynImpl* myallt = new MyDynImpl();
    std::cerr << "@@4" << std::endl;
    
    PortableServer::ObjectId_var myalltid = poa->activate_object(myallt);
    std::cerr << "@@5" << std::endl;

    obj = poa->id_to_reference (myalltid.in ());

    std::cerr << "@@6" << std::endl;

    CORBA::String_var sior(orb->object_to_string(obj.in ()));
    cerr << "'" << (char*) sior << "'" << endl;
    
    myallt->_remove_ref();

    PortableServer::POAManager_var pman = poa->the_POAManager();
    pman->activate();

    std::cerr << "Server ready" << std::endl;
    orb->run();

  }

  catch(...)
    {
      cerr << "fatal error : exception raised by the server" << endl;
    }

  return EXIT_SUCCESS;
}
