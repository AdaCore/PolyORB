// $Id: //droopi/main/testsuite/corba/interop/cpp/TAO/dynserver_mt_TAO.cc#2 $

// DSI server, implements echoULong method. It uses TAO's worker thread pool.
// NOTE: this server will compile only with TAO

#include <iostream>
#include <stdlib.h>
#include <stdio.h>

#include "ace/pre.h"
#include "tao/DynamicInterface/Dynamic_Implementation.h"
#include "tao/DynamicInterface/Server_Request.h"
#include "tao/PortableServer/PortableServer.h"
#include "tao/corba.h"
#include "ace/Task.h"
#include "ace/post.h"
#include "tao/AnyTypeCode/NVList.h"
#include "tao/AnyTypeCode/SystemExceptionA.h"
#include "tao/AnyTypeCode/TypeCode.h"
#include "tao/AnyTypeCode/TypeCode_Constants.h"

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

//////////////////////////////////////////////////////////////////////////
// Worker thread implementation

class Worker : public ACE_Task_Base
{
public:
  Worker (CORBA::ORB_ptr orb);

  virtual int svc (void);

private:
  CORBA::ORB_var orb_;
};

Worker::Worker (CORBA::ORB_ptr orb)
  :  orb_ (CORBA::ORB::_duplicate (orb))
{
}

int
Worker::svc (void)
{
  ACE_DECLARE_NEW_CORBA_ENV;
  ACE_TRY
    {
      this->orb_->run (ACE_ENV_SINGLE_ARG_PARAMETER);
      ACE_TRY_CHECK;
    }
  ACE_CATCHANY
    {
    }
  ACE_ENDTRY;
  return 0;
}

//////////////////////////////////////////////////////////////////////////

void
MyDynImpl::invoke(CORBA::ServerRequest_ptr request)
{
  try {

    if (ACE_OS::strcmp ("_is_a", request->operation ()) == 0)
      {
	CORBA::NVList_ptr list;
	orb->create_list (0, list);
	
	CORBA::Any type_id;
	type_id._tao_set_typecode (CORBA::_tc_string);

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
	
	CORBA::Any a;
	a._tao_set_typecode (CORBA::_tc_ulong);

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
	throw CORBA::BAD_OPERATION(0, CORBA::COMPLETED_NO);
      }
  }
  catch(CORBA::SystemException& ex){
    std::cout << "I'll be back" << std::endl;
    //    CORBA::Any a;
    // a <<= ex;
    // request->set_exception(a);
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

//////////////////////////////////////////////////////////////////////////
// main function

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

    Worker worker (orb.in ());
      if (worker.activate (THR_NEW_LWP | THR_JOINABLE,
                           4) != 0)
        ACE_ERROR_RETURN ((LM_ERROR,
                           "Cannot activate client threads\n"),
                          1);

      worker.thr_mgr ()->wait ();
  }

  catch(...)
    {
      cerr << "fatal error : exception raised by the server" << endl;
    }

  return EXIT_SUCCESS;
}
