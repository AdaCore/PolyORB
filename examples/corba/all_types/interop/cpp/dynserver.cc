#include <iostream>
#include <stdlib.h>
#include "all_types.hh"
#include <stdio.h>

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
    if( strcmp(request->operation(), "echoULong") )
      throw CORBA::BAD_OPERATION(0, CORBA::COMPLETED_NO);

    CORBA::NVList_ptr args;
    orb->create_list(0, args);
    CORBA::Any a;
    a.replace(CORBA::_tc_ulong, 0);
    args->add_value("", a, CORBA::ARG_IN);

    request->arguments(args);

    CORBA::ULong x;
    *(args->item(0)->value()) >>= x;

    CORBA::Any* result = new CORBA::Any();
    *result <<= x;
    request->set_result(*result);
    delete result;
  }
  catch(CORBA::SystemException& ex){
    CORBA::Any a;
    a <<= ex;
    request->set_exception(a);
  }
  catch(...){
    cout << "echo_dsiimpl: MyDynImpl::invoke - caught an unknown exception."
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
  // See omniorb documentation for explanations
  try {
    orb = CORBA::ORB_init(argc, argv);
    std::cerr << "@@1" << std::endl;

    CORBA::Object_var obj = orb->resolve_initial_references("RootPOA");
    std::cerr << "@@2" << std::endl;

    PortableServer::POA_var poa = PortableServer::POA::_narrow(obj);
    std::cerr << "@@3" << std::endl;

    MyDynImpl* myallt = new MyDynImpl();
    std::cerr << "@@4" << std::endl;
    
    PortableServer::ObjectId_var myalltid = poa->activate_object(myallt);
    std::cerr << "@@5" << std::endl;

    // obj = myallt->_this();
    obj = poa->create_reference_with_id(myalltid, "IDL:all_types:1.0");

    std::cerr << "@@6" << std::endl;

    CORBA::String_var sior(orb->object_to_string(obj));
    cerr << "'" << (char*) sior << "'" << endl;
    
    myallt->_remove_ref();

    PortableServer::POAManager_var pman = poa->the_POAManager();
    pman->activate();

    orb->run();
  }

  catch(...)
    {
      cerr << "fatal error : exception raised by the server" << endl;
    }

  return EXIT_SUCCESS;
}
