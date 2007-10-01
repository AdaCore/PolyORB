#include <unistd.h>
#include <iostream>
#include "report.cc"

#ifdef __USE_OMNIORB__
#include <all_functions.hh>
#endif

#ifdef __USE_MICO__
#include <all_functions.h>
#endif

using namespace std;

int main(int argc, char* argv[]) 
{

  if (argc != 2) 
    {
      printf("Usage : mico-client <server IOR>\n");
    }

  string ior = argv[1];
  
  CORBA::ORB_var _orb = CORBA::ORB_init(argc, argv);
  CORBA::Object_var obj = _orb->string_to_object(ior.c_str());
  all_functions_var allfunc = all_functions::_narrow(obj);

  new_test ("Different invocation modes");
  
  if (CORBA::is_nil(allfunc)) 
    {
      cerr << "can't access object" << endl;
      exit (1);
    }

  allfunc->oneway_void_proc();
  sleep(1);

  bool ok = (allfunc->oneway_checker() == 1);

  if (ok) 
    {
      sleep(5);
      ok = (allfunc->oneway_checker() == 2);
    }

  output ("test void one way procedure", ok);
  end_report ();
}

