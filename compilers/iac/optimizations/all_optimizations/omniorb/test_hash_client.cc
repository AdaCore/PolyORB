#include "report.cc"

#ifdef __USE_OMNIORB__
#include <test_hash.hh>
#endif

#include "n_iterations.hh"
#include <sys/time.h>

static void test(test_hash_ptr p)
{
  new_test ("CORBA Types");

  output("testing not null", !p->_is_nil ());

  //  Declare the table of subprogram pointers

  typedef CORBA::Long (_objref_test_hash::*echoLongPtr) (CORBA::Long);

  echoLongPtr echoLongTable [100] =
    {&_objref_test_hash::echoLong00,
     &_objref_test_hash::echoLong01,
     &_objref_test_hash::echoLong02,
     &_objref_test_hash::echoLong03,
     &_objref_test_hash::echoLong04,
     &_objref_test_hash::echoLong05,
     &_objref_test_hash::echoLong06,
     &_objref_test_hash::echoLong07,
     &_objref_test_hash::echoLong08,
     &_objref_test_hash::echoLong09,
     &_objref_test_hash::echoLong10,
     &_objref_test_hash::echoLong11,
     &_objref_test_hash::echoLong12,
     &_objref_test_hash::echoLong13,
     &_objref_test_hash::echoLong14,
     &_objref_test_hash::echoLong15,
     &_objref_test_hash::echoLong16,
     &_objref_test_hash::echoLong17,
     &_objref_test_hash::echoLong18,
     &_objref_test_hash::echoLong19,
     &_objref_test_hash::echoLong20,
     &_objref_test_hash::echoLong21,
     &_objref_test_hash::echoLong22,
     &_objref_test_hash::echoLong23,
     &_objref_test_hash::echoLong24,
     &_objref_test_hash::echoLong25,
     &_objref_test_hash::echoLong26,
     &_objref_test_hash::echoLong27,
     &_objref_test_hash::echoLong28,
     &_objref_test_hash::echoLong29,
     &_objref_test_hash::echoLong30,
     &_objref_test_hash::echoLong31,
     &_objref_test_hash::echoLong32,
     &_objref_test_hash::echoLong33,
     &_objref_test_hash::echoLong34,
     &_objref_test_hash::echoLong35,
     &_objref_test_hash::echoLong36,
     &_objref_test_hash::echoLong37,
     &_objref_test_hash::echoLong38,
     &_objref_test_hash::echoLong39,
     &_objref_test_hash::echoLong40,
     &_objref_test_hash::echoLong41,
     &_objref_test_hash::echoLong42,
     &_objref_test_hash::echoLong43,
     &_objref_test_hash::echoLong44,
     &_objref_test_hash::echoLong45,
     &_objref_test_hash::echoLong46,
     &_objref_test_hash::echoLong47,
     &_objref_test_hash::echoLong48,
     &_objref_test_hash::echoLong49,
     &_objref_test_hash::echoLong50,
     &_objref_test_hash::echoLong51,
     &_objref_test_hash::echoLong52,
     &_objref_test_hash::echoLong53,
     &_objref_test_hash::echoLong54,
     &_objref_test_hash::echoLong55,
     &_objref_test_hash::echoLong56,
     &_objref_test_hash::echoLong57,
     &_objref_test_hash::echoLong58,
     &_objref_test_hash::echoLong59,
     &_objref_test_hash::echoLong60,
     &_objref_test_hash::echoLong61,
     &_objref_test_hash::echoLong62,
     &_objref_test_hash::echoLong63,
     &_objref_test_hash::echoLong64,
     &_objref_test_hash::echoLong65,
     &_objref_test_hash::echoLong66,
     &_objref_test_hash::echoLong67,
     &_objref_test_hash::echoLong68,
     &_objref_test_hash::echoLong69,
     &_objref_test_hash::echoLong70,
     &_objref_test_hash::echoLong71,
     &_objref_test_hash::echoLong72,
     &_objref_test_hash::echoLong73,
     &_objref_test_hash::echoLong74,
     &_objref_test_hash::echoLong75,
     &_objref_test_hash::echoLong76,
     &_objref_test_hash::echoLong77,
     &_objref_test_hash::echoLong78,
     &_objref_test_hash::echoLong79,
     &_objref_test_hash::echoLong80,
     &_objref_test_hash::echoLong81,
     &_objref_test_hash::echoLong82,
     &_objref_test_hash::echoLong83,
     &_objref_test_hash::echoLong84,
     &_objref_test_hash::echoLong85,
     &_objref_test_hash::echoLong86,
     &_objref_test_hash::echoLong87,
     &_objref_test_hash::echoLong88,
     &_objref_test_hash::echoLong89,
     &_objref_test_hash::echoLong90,
     &_objref_test_hash::echoLong91,
     &_objref_test_hash::echoLong92,
     &_objref_test_hash::echoLong93,
     &_objref_test_hash::echoLong94,
     &_objref_test_hash::echoLong95,
     &_objref_test_hash::echoLong96,
     &_objref_test_hash::echoLong97,
     &_objref_test_hash::echoLong98,
     &_objref_test_hash::echoLong99};

  bool Pass = true;
  int j;
  timeval t1, t2;
  double duration, d1, d2;

  for (int i = 0; i < N_ITERATIONS; i++) {
    j = i % 100;
    gettimeofday(&t1, NULL);
    Pass = Pass && ((p->*echoLongTable[j])(123) == 123);
    gettimeofday(&t2, NULL);
    d1 = t1.tv_sec + (t1.tv_usec / 1000000.0);
    d2 = t2.tv_sec + (t2.tv_usec / 1000000.0);
    duration = duration + (d2 - d1);
  }

  output("testing Long", Pass);
  cerr << "tests completed." << endl;
  cerr << "Duration : " << duration << " sec" << endl;
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
      
      test_hash_var alltref = test_hash::_narrow(obj);
      
      if (CORBA::is_nil(alltref))
	{
	  cerr << "can't access object" << endl;
	  exit(EXIT_FAILURE);
	}
      
      test(alltref);

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
