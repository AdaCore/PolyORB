// $Id: //droopi/main/testsuite/corba/interop/cpp/common/all_types_dynclient.cc#2 $
// DII client, makes multiple calls to echoULong method

#include <stdlib.h>
#include <iostream>
#include "report.cc"

#ifdef __USE_TAO__
#include "tao/DynamicInterface/Request.h"
#include "tao/corba.h"
#include "tao/AnyTypeCode/Any.h"
#include "tao/AnyTypeCode/TypeCode_Constants.h"
#include "tao/AnyTypeCode/TypeCode_Constants.h"
#endif

#ifdef __USE_OMNIORB__
#include "omniORB4/CORBA.h"
#endif

#ifdef __USE_MICO__
#include <CORBA.h>
#endif

using namespace std;
using namespace CORBA;

static void test(CORBA::Object_var p)
{
    int Pass = 1;
    for (int i = 0; i < 1000; i++) {
       CORBA::ULong arg = 234;
       CORBA::Request_var req1 = p->_request("echoULong");
       req1->add_in_arg() <<= arg;
       req1->set_return_type(CORBA::_tc_ulong);
       req1->invoke(); 

       if ( req1->exceptions() != NULL && req1->exceptions()->count() > 0) {
         cerr<< "all_types_dynclient: An exception was thrown!" << endl;
       	 return;
       }

       CORBA::ULong ret1;
       req1->return_value() >>= ret1;

       Pass &= (ret1 == 234);
    }
    output ("testing ULong (1000 times)", Pass);
    end_report();
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
  catch(CORBA::COMM_FAILURE&)
    {
      cerr << "Caught system exception COMM_FAILURE -- unable to contact the object" << endl;
    }
  catch(CORBA::SystemException& ex)
    {
#if defined (__USE_TAO__) || (__USE_MICO__)
      cerr << "Caught a CORBA::SystemException: " << ex << endl;
#endif

#if defined (__USE_OMNIORB__)
      cerr << "Caught a CORBA::SystemException: " << endl;
#endif

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

