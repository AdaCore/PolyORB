//----------------------------------------------------------------------------
//                                                                          --
//                          ADABROKER COMPONENTS                            --
//                                                                          --
//                               C L I E N T                                --
//                                                                          --
//                                 B o d y                                  --
//                                                                          --
//         Copyright (C) 1999-2000 ENST Paris University, France.           --
//                                                                          --
// AdaBroker is free software; you  can  redistribute  it and/or modify it  --
// under terms of the  GNU General Public License as published by the  Free --
// Software Foundation;  either version 2,  or (at your option)  any  later --
// version. AdaBroker  is distributed  in the hope that it will be  useful, --
// but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
// TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
// License  for more details.  You should have received  a copy of the GNU  --
// General Public License distributed with AdaBroker; see file COPYING. If  --
// not, write to the Free Software Foundation, 59 Temple Place - Suite 330, --
// Boston, MA 02111-1307, USA.                                              --
//                                                                          --
//             AdaBroker is maintained by ENST Paris University.            --
//                     (email: broker@inf.enst.fr)                          --
//                                                                          --
//----------------------------------------------------------------------------

// This is the All_Types interop client. The object reference is given as a
// stringified IOR on the command line.
//
// Usage: client <object reference>

#include <iostream.h>
#include <string.h>
#include "all_types.hh"

#define Max 60

void
Output (char *s, bool pass) {
  char Line [Max + 1];
  int len = strlen (s);
  int i;

  for (i = 0 ; i < sizeof Line - 1; i++) {
    Line [i] = (i < len) ? s [i] : '.';
  }
  Line [sizeof Line - 1] = '\0';

  cout << Line << (pass ? " : PASSED" : " : FAILED") << endl;
}

bool True = 1;
  
int
main (int argc, char **argv) 
{
  CORBA::ORB_ptr orb = CORBA::ORB_init(argc,argv,"omniORB2");

  if (argc < 2) {
    cerr << "usage: client <object reference>" << endl;
    return 1;
  }

  try {
    CORBA::Object_var obj = orb->string_to_object(argv[1]);
    all_types_var at = all_types::_narrow(obj);

    // Here comes the hot chou-fleur
    if (all_types_Helper::is_nil (at)) {
      cout << "main : cannot invoke on a nil reference" << endl;
      return -1;
    }

    Output ("test not null", ! all_types_Helper::is_nil (at));

    for (;;) {
      Output ("test boolean", at->echoBoolean (True) == True);
      Output ("test short", at->echoShort (123) == 123);
      Output ("test long",  at->echoLong (456) == 456);
      Output ("test unsigned_short", at->echoUShort (456) == 456);
      Output ("test unsigned_long", at->echoULong (123) == 123);
      Output ("test float", at->echoFloat ((CORBA::Float)2.7)
	      == (CORBA::Float)2.7);
      Output ("test double", at->echoDouble (3.14) == 3.14);
      Output ("test char", at->echoChar ('A') == 'A');
      Output ("test octet", at->echoOctet (5) == 5);

      Output ("test string", !strcmp (at->echoString ("hello"), "hello"));
      Output ("test enum", at->echoColor (all_types::Blue) == all_types::Blue);

      // fixed point NIY.

      {
	struct all_types::simple_struct Test_Struct;
	struct all_types::simple_struct Copy_Struct;

	Test_Struct.a = 123;
	Test_Struct.s = "foobar";

	Copy_Struct = *at->echoStruct (Test_Struct);

	Output ("test struct",
		((Copy_Struct.a == Test_Struct.a)
		 && !strcmp (Copy_Struct.s, Test_Struct.s)));
      }

      {
	struct all_types::array_struct Test_Struct;
	struct all_types::array_struct Copy_Struct;
	bool Pass = 1;
	int i;

	for (i = 0 ; i < 10 ; i++)
	  Test_Struct.a[i] = 3 * i + 7;
	Test_Struct.b = 12345;

	Copy_Struct = at->echoArrayStruct (Test_Struct);

	for (i = 0 ; i < 10 ; i++)
	  Pass &= (Test_Struct.a[i] == Copy_Struct.a[i]);
	Pass &= (Copy_Struct.b == Test_Struct.b);

	Output ("test array struct", Pass);
      }

      {
	all_types::myUnion Test_Unions[4];
	all_types::myUnion Copy_Union;

	Test_Unions [0].Unknown (987);
	Test_Unions [1].Counter (123);
	Test_Unions [2].Flag (1);
	Test_Unions [3].Hue (all_types::Red);

	bool Pass = 1;
	int i;
	
	for (i = 0 ; i < 4 ; i++) {
	  Copy_Union = at->echoUnion (Test_Unions [i]);

	  if (!(Pass &= Copy_Union._d ()
		== Test_Unions [i]._d ()))
	    break;

	  switch (Copy_Union._d ()) {
	  case 1:
	    Pass &= Copy_Union.Counter () == Test_Unions [i].Counter ();
	    break;
	  case 2:
	    Pass &= Copy_Union.Flag () == Test_Unions [i].Flag ();
	    break;
	  case 3:
	    Pass &= Copy_Union.Hue () == Test_Unions [i].Hue ();
	    break;
	  default:
	    Pass &= Copy_Union.Unknown () == Test_Unions [i].Unknown ();
	    break;
	  }

	  if (!Pass)
	    break;
	}
	
	Output ("test union", Pass);
      }

      {
	CORBA::Short Tab[] = { 16, 32, 64, 128, 257 };
	all_types::U_sequence X (5, 5, Tab);
	all_types::U_sequence Y;
	bool Pass = 1;
	int i;

	Y = *at->echoUsequence (X);
	
	Pass &= Y.length () == X.length ();

	for (i = 0 ; Pass && i < X.length () ; i++) {
	  Pass &= (X [i] == Y [i]);
	}
	
	Output ("test unbounded sequence", Pass);
      }

      {
	bool Pass = 0;

	try {
	  at->testException (2485);
	} catch (all_types::my_exception& ex) {
	  Pass = (ex.info == 2485);
	} catch (...) {
	  /* Nothing. */
	}
	Output ("test exception", Pass);
      }

      {
	all_types::simple_array  X = { 2, 3, 5, 7, 11 };
	all_types::simple_array_var Y;
	bool Pass = 1;
	int i;
	
	Y = at->echoArray (X);
	
	for (i = 0 ; Pass && i < 5 ; i++) {
	  Pass &= X [i] == Y [i];
	}
	
	Output ("test simple array", Pass);
      }
      
      {
         all_types::matrix M = { { 0xfe, 0xe1, 0x5a },
		      { 0xde, 0xad, 0xa5 },
		      { 0xbe, 0xef, 0x88 } };
	 all_types::matrix_var N;
	 int i, j;
	 bool Pass = 1;

	 N = at->echoMatrix (M);
#define DEBUG 0
#if DEBUG
#define Continue_If_Pass 1
#else
#define Continue_If_Pass Pass
#endif
	 for (i = 0 ; Continue_If_Pass && i < 3 ; i++)
	   for (j = 0 ; Continue_If_Pass && j < 3 ; j++) {
	     Pass &= M [i][j] == N [i][j];
#if DEBUG
	     cout << "M[" << i << " ][" << j << "] == " << M[i][j] << endl;
	     cout << "N[" << i << " ][" << j << "] == " << N[i][j] << endl;
#endif
	   }

         Output ("test multi-dimensional array", Pass);
      }

      at->myColor (all_types::Green);
      Output ("test attribute", at->myColor () == all_types::Green);

      {
	CORBA::Long Counter_First_Value
           = at->Counter ();
	CORBA::Long Counter_Second_Value
           = at->Counter ();

	Output ("test read-only attribute",
                Counter_Second_Value == Counter_First_Value + 1);
      }

      {
	all_types_var X;

	X = at->echoRef (at);
	Output ("test self reference", X->echoLong (31337) == 31337);
      }

      // The end.
      break;
    }
  } catch(CORBA::COMM_FAILURE& ex) {
    cerr << "Caught system exception COMM_FAILURE, unable to contact the "
         << "object." << endl;
  } catch(omniORB::fatalException& ex) {
    cerr << "Caught omniORB2 fatalException. This indicates a bug is caught "
         << "within omniORB2.\nPlease send a bug report.\n"
         << "The exception was thrown in file: " << ex.file() << "\n"
         << "                            line: " << ex.line() << "\n"
         << "The error message is: " << ex.errmsg() << endl;
  } catch(...) {
    cerr << "Caught a system exception." << endl;
  }

  orb->NP_destroy();

  return 0;
}
