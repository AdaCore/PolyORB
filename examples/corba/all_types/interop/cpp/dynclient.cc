// $Id$
// DII client, makes 10 000 calls to echoULong method

#include <stdlib.h>
#include <iostream>

#ifdef __USE_TAO__
#include "tao/DynamicInterface/Request.h"
#include "tao/corba.h"
#endif

#ifdef __USE_OMNIORB__
#include "omniORB4/CORBA.h"
#endif

using namespace std;
using namespace CORBA;

static void print(bool Pass)
{
  cerr << (Pass ? "PASSED" : "FAILED") << endl;
}

static void test(CORBA::Object_var p)
{
  cerr << "performing the tests" << endl;
  cerr << "--------------------" << endl << endl;

    cerr << "testing ULong "  << "\t\t" << ": "  << "\t";
    int Pass = 1;
    for (int i = 0; i < 10000; i++) {
       CORBA::ULong arg = 234;
       CORBA::Request_var req1 = p->_request("echoULong");
       req1->add_in_arg() <<= arg;
       req1->set_return_type(CORBA::_tc_ulong);
       req1->invoke(); 

       if ( req1->exceptions() != NULL && req1->exceptions()->count() > 0) {
         cerr<< "dynclient: An exception was thrown!" << endl;
       	 return;
       }

       CORBA::ULong ret1;
       req1->return_value() >>= ret1;

       Pass &= (ret1 == 234);
    }
    print(Pass);
}

int main(int argc, char** argv)
{
  try
    {
      CORBA::ORB_var orb = CORBA::ORB_init(argc, argv);

      if (argc != 2)
        {
          cerr << "wrong number of param." << endl;
          exit(EXIT_FAILURE);
        }

      CORBA::Object_var obj = orb->string_to_object(argv[1]);

      if (CORBA::is_nil(obj.in ()))
        {
          cerr << "can't access object" << endl;
          exit(EXIT_FAILURE);
        }

      test(obj);

      orb->destroy();

    }
  catch(CORBA::COMM_FAILURE& ex)
    {
      cerr << "Caught system exception COMM_FAILURE -- unable to contact the object" << endl;
    }
  catch(CORBA::SystemException&)
    {
      cerr << "Caught a CORBA::SystemException" << endl;
    }
  catch(CORBA::Exception&)
    {
      cerr << "Caught CORBA::Exception" << endl;
    }
  catch(...)
    {
      cerr << "Caught unknown error" << endl;
    }
  return 0;
}

