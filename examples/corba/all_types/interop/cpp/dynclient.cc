#include <stdlib.h>
#include <iostream>
#include <omniORB4/CORBA.h>

using namespace std;

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
       req1->invoke(); // invocation de la methode


       if( req1->env()->exception() ) {
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

      if (CORBA::is_nil(obj))
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
  catch(omniORB::fatalException& fe)
    {
      cerr << "Caught omniORB::fatalException:" << endl;
      cerr << "  file : " << fe.file() << endl;
      cerr << "  line : " << fe.line() << endl;
      cerr << "  mesg : " << fe.errmsg() << endl;
    }
  catch(...)
    {
      cerr << "Caught unknown error" << endl;
    }
  return 0;
}

