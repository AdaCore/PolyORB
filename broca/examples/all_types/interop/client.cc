// client.cc - This is the source code of example 2 used in Chapter 2
//              "The Basics" of the omniORB2 user guide.
//
//              This is the client. The object reference is given as a
//              stringified IOR on the command line.
//
// Usage: client <object reference>
//
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

      {
	struct all_types::simple_struct Test_Struct;
	struct all_types::simple_struct Copy_Struct;
	bool Pass = 1;
	int i;

	for (i = 0; i < 10; i++)
	  Test_Struct.a[i] = 2 * (i + 1);
	Test_Struct.s = "foobar";

	Copy_Struct = *at->echoStruct (Test_Struct);

	for (i = 0 ; i < 10 ; i++) {
	  if (!(Pass &= (Copy_Struct.a [i] == Test_Struct.a [i])))
	    break;
	}

	Pass &= !strcmp (Copy_Struct.s, Test_Struct.s);

	Output ("test struct", Pass);
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

      /*	
	declare
	Member : My_Exception_Members;
      begin
         Ok := False;
         TestException (Myall_types, 2485);
      exception
         when E : My_Exception =>
            Get_Members (E, Member);
            Ok := (Member.Info = 2485);
      end;
      Output ("test exception", Ok);
      */

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
	 for (i = 0 ; /* Pass && */ i < 3 ; i++)
	   for (j = 0 ; /* Pass && */ j < 3 ; j++) {
	     Pass &= M [i][j] == N [i][j];
	     cout << "M[" << i << " ][" << j << "] == " << M[i][j] << endl;
	     cout << "N[" << i << " ][" << j << "] == " << N[i][j] << endl;
	   }

         Output ("test multi-dimensional array", Pass);
      }


      /*
--   declare
--      X : Simple_Struct := (A => (0,1,2,3,4,5,6,7,8,9), B => 10);
--   begin
--      Output ("test simple structure", at->Echo2 (Myall_types, X) = X);
--   end;

--   Output ("test enumeration", at->Echo3 (Myall_types, Blue) = Blue);
--
   -- bounded sequences
--   declare
--      X : B_Sequence := B_Sequence (IDL_SEQUENCE_Long_1.Null_Sequence);
--   begin
--      X := X & 1 & 2 & 3 & 4 & 5;
--      Output ("test bounded sequence",  at->Echo7 (Myall_types, X) = X);
--   end;

--   declare
--      X : all_types.Line
--        := ((Switch => 1, Counter => 19),
--            (Switch => 2, Flags => True),
--            (Switch => 3, Unknown => 25));
--   begin
--      Output ("test arrays (1)", at->Echo8 (Myall_types, X) = X);
--   end;
--
--   declare
--      X : Square
--        := (((A => (0,1,2,3,4,5,6,7,8,9), B=> 23),
--             (A => (9,8,7,6,5,4,3,2,1,0), B=> 17)),
--            ((A => (0,1,2,3,4,5,6,7,8,9), B=> 23),
--             (A => (9,8,7,6,5,4,3,2,1,0), B=> 17)));
--   begin
--      Output ("test arrays (2)", at->Echo9 (Myall_types, X) = X);
--   end;
--
--   declare
--      X : Cube
--        := (((To_CORBA_String (Standard.String'("case1")),
--              To_CORBA_String (Standard.String'("case2"))),
--             (To_CORBA_String (Standard.String'("case3")),
--              To_CORBA_String (Standard.String'("case4")))),
--            ((To_CORBA_String (Standard.String'("case5")),
--              To_CORBA_String (Standard.String'("case6"))),
--             (To_CORBA_String (Standard.String'("case7")),
--              To_CORBA_String (Standard.String'("case8")))));
--   begin
--      Output ("test arrays (3)", at->Echo10 (Myall_types, X) = X);
--   end;

--   declare
--      X : all_types.Ref;
--      Y : Example := (Switch => 2, Flags => False);
--   begin
--      Set_N_Attribute (Myall_types, Y);
--      X := at->Echo11 (Myall_types, Myall_types);
--      Output ("test reference", Get_N_Attribute (X) = Y);
--   end;

--   declare
--      X : CORBA.Object.Ref := CORBA.Object.Ref (To_Ref (Myall_types));
--   begin
--      Output ("test CORBA.Object.Ref",
--         Is_Equivalent (at->Echo12 (Myall_types, X), X));
--   end;

      Set_MyColor (Myall_types, Green);
      Output ("test attribute", Get_MyColor (Myall_types) = Green);
      declare
         Counter_First_Value : CORBA.Long
           := Get_Counter (Myall_types);
         Counter_Second_Value : CORBA.Long
           := Get_Counter (Myall_types);
      begin
         Output ("test read-only attribute",
                 Counter_Second_Value = Counter_First_Value + 1);
      end;

      --      declare
      --         X : all_types.Ref;
      --      begin
      --         X := at->EchoRef (Myall_types, Myall_types);
      --         Output ("test self reference", at->EchoLong (X, 31337) = 31337);
      --      end;
   end loop;

     */
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
