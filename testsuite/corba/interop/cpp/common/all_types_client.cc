#include "report.cc"

#ifdef __USE_OMNIORB__
#include <all_types.hh>
#endif

#ifdef __USE_MICO__
#include <all_types.h>
#endif


//#define DEBUG     

#define BOOLEAN   
#define SHORT     
#define LONG      
#define USHORT    
#define ULONG
#define FLOAT
#define DOUBLE
#define CHAR
#define OCTET
#define STRING
#define ENUM
#define BSEQ
#define USEQ
#define ARRAY
#define MATRIX
#define BIGMATRIX
#define STRUCT
#define ARRAY_STRUCT
#define UNION
#define ATTRIBUTE
#define OBJECT
#define MONEY
#define EXCEPTION

static void test(all_types_ptr p)
{
  new_test ("CORBA Types");

  output("testing not null", !p->_is_nil ());

  // type simple
  {
    bool Pass;

    //boolean
#ifdef BOOLEAN
    Pass = (p->echoBoolean(true) == true);
    output("testing Boolean", Pass);
#endif

    //short
#ifdef SHORT
    Pass = (p->echoShort(123) == 123);
    output("testing Short", Pass);
#endif

    //long
#ifdef LONG
    Pass = (p->echoLong(456) == 456);
    output("testing Long", Pass);
#endif

    //ushort
#ifdef USHORT
    Pass = (p->echoUShort(456) == 456);
    output("testing UShort", Pass);
#endif

    //ulong
#ifdef ULONG
    Pass = (p->echoULong(123) == 123);
    output("testing ULong", Pass);
#endif

    //float
#ifdef FLOAT
    Pass = (p->echoFloat(2.7) == (float)2.7);
    output("testing Float", Pass);

    // Special value: becomes an invalid representation when byte swapped
    Pass = (p->echoFloat(34.4072222) == (float)34.4072222);
    output("testing Float inval-if-swapped", Pass);
#endif

    //double
#ifdef DOUBLE
    Pass = (p->echoDouble(1.5) == 1.5);
    output("testing Double", Pass);
#endif

    //char
#ifdef CHAR
    Pass = (p->echoChar('A') == 'A');
    output( "testing Char", Pass);
#endif

    //Octet
#ifdef OCTET
    Pass = (p->echoOctet(5) == 5);
    output("testing Octet", Pass);
#endif

    //String 
#ifdef STRING
    Pass = !strcmp(p->echoString("hello"), "hello");
    output("testing String", Pass);
#endif
  }

  // type complexe : 
  {

    // Enum
#ifdef ENUM
    bool Pass;
    Pass = (p->echoColor(all_types::Blue) == all_types::Blue);
    output("testing Enum", Pass);
#endif
  }

  // Sequence 
  {
#ifdef USEQ
    CORBA::Short Tab[] = { 16, 32, 64, 128, 257 };
    all_types::U_sequence X (5, 5, Tab);
    all_types::U_sequence Y;
    bool Pass = 1;
    
    Y = *p->echoUsequence(X);
    
    Pass &= Y.length() == X.length();
#ifdef DEBUG
    output("comparing the length" , Pass);
#endif

    for (unsigned int i = 0 ; Pass && i < X.length() ; i++) 
      {
	Pass &= (X[i] == Y[i]);
#ifdef DEBUG
	cerr << X[i] << "==" << Y[i] << endl;
#endif
	
      }
    output("testing Sequence", Pass);
#endif
  }

  // Sequence 
  {
#ifdef BSEQ
    all_types::B_sequence X;
    all_types::B_sequence Y;
    bool Pass = 1;

    X.length(5);

    for (int i = 0; i < 5; i++)
      X[i] = i;

    Y = *p->echoBsequence(X);
    
    Pass &= Y.length() == X.length();
    output("comparing the length", Pass);
 
    for (unsigned int i = 0 ; Pass && i < X.length() ; i++) 
      {
	Pass &= (X[i] == Y[i]);
#ifdef DEBUG
	cerr << X[i] << "==" << Y[i] << endl;
#endif
	
      }
    output("testing Bounded Sequence", Pass);
#endif
  }    

   // simple array 
  {
#ifdef ARRAY
    all_types::simple_array A = { 2, 3, 5, 7, 11 };
    all_types::simple_array_var B = p->echoArray(A);
    
    bool Pass = 1;
    for (int i = 0; i <= 4; i++) 
      {
	Pass &= (A[i] == ((CORBA::Long*)B)[i]); 
#ifdef DEBUG
	cerr << A[i] << B[i] << endl;
#endif
      }

    output("testing Simple Array", Pass);
#endif
  }
  
  // matrix
#ifdef MATRIX
  {
    all_types::matrix A = { { 0xfe, 0xe1, 0x5a },
		            { 0xde, 0xad, 0xa5 },
		            { 0xbe, 0xef, 0x88 } };
    all_types::matrix_var B = p->echoMatrix(A);

    bool Pass = 1;

    for (int i = 0; i < 3; i++)
      {
	for (int j = 0; j < 3 ; j++)
	  Pass &= (A[i][j] == ((CORBA::Long(*)[3])B)[i][j]);
      }
    
    output("testing Matrix", Pass);
  }
#endif

  // big matrix
#ifdef BIGMATRIX
  {
    all_types::bigmatrix A;
    all_types::bigmatrix_var B; 
    bool Pass = true;

    for (int i = 0; i < 30; i++)
      for (int j = 0; j < 15; j++)
	A[i][j] = ((i + 1) * (j + 2));

    B = p->echoBigMatrix(A);

    for (int i = 0; i < 30; i++)
      for (int j = 0; j < 15; j++)
	  Pass &= (A[i][j] == ((CORBA::Long(*)[15])B)[i][j]);
    
    output("testing big matrix", Pass);
  }
#endif

  // structure
#ifdef STRUCT
 {
   struct all_types::simple_struct Test_Struct;
   struct all_types::simple_struct Copy_Struct;
   bool Pass = false;

   Test_Struct.a = 123;
   Test_Struct.s = "foobar";
   
   Copy_Struct = *p->echoStruct (Test_Struct);
   
   Pass = ((Copy_Struct.a == Test_Struct.a) && !strcmp (Copy_Struct.s, Test_Struct.s));
   output("testing Struct", Pass);
 }
#endif
  
  // array struct
#ifdef ARRAY_STRUCT
  {
    struct all_types::array_struct Test_Struct;
    struct all_types::array_struct Copy_Struct;
    bool Pass = 1;
    int i;
    

    for (i = 0 ; i < 10 ; i++)
      Test_Struct.a[i] = 3 * i + 7;
    Test_Struct.b = 12345;
    
    Copy_Struct = p->echoArrayStruct (Test_Struct);
    
    for (i = 0 ; i < 10 ; i++)
      Pass &= (Test_Struct.a[i] == Copy_Struct.a[i]);
    Pass &= (Copy_Struct.b == Test_Struct.b);
    output("test array struct", Pass);    
  }
#endif

 // Union
#ifdef UNION
 {
   all_types::myUnion Test_Unions[4];
   all_types::myUnion Copy_Union;
   
   Test_Unions [0].Unknown (987);
   Test_Unions [1].Counter (123);
   Test_Unions [2].Flag (1);
   Test_Unions [3].Hue (all_types::Red);
   
   bool Pass = 1;
   int i;
   
   for (i = 0 ; i < 4 ; i++) 
     {
       Copy_Union = p->echoUnion (Test_Unions [i]);
       
       if (!(Pass &= Copy_Union._d () == Test_Unions [i]._d ()))
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
   output("test union", Pass);
 }
#endif


  // Union Switch
#ifdef UNION_SWITCH
 {
   all_types::myUnionEnumSwitch Test_UnionES[4];
   all_types::myUnionEnumSwitch Copy_UnionES;
   
   Test_UnionES[0].Unknown (987);
   Test_UnionES[1].Counter (123);
   Test_UnionES[2].Flag (1);
   Test_UnionES[3].Hue (all_types::Red);

   //          := ((Switch => Red, Foo => 31337),
   //              (Switch => Green, Bar => 534),
   //              (Switch => Blue, Baz => CORBA.To_CORBA_String ("grümpf")));
   bool Pass : Boolean := True;                                                                                                                                     
//    for I in Test_Unions'Range loop    
//               Pass := Pass
//                 and then echoUnionEnumSwitch (Myall_types, Test_Unions (I))
//                 = Test_Unions (I);
//               exit when not Pass;
//            end loop;
//            Output ("test union with enum switch", Pass);
//         end;                                          
 }
#endif

 // attribute
#ifdef ATTRIBUTE
 {
   bool Pass;

   p->myColor(all_types::Green);
   Pass = (p->myColor() == all_types::Green);
   
   output("test attribute", Pass);

   CORBA::Long Counter_First_Value = p->Counter();
   CORBA::Long Counter_Second_Value = p->Counter();
   
   output("test read-only attribute", 
	  Counter_Second_Value == Counter_First_Value + 1);

 }
#endif

 // object
#ifdef OBJECT
 {
   all_types_var X;

   X = all_types::_narrow(p->echoRef(p));

   output("test self reference", X->echoLong(17) == 17);

   for(int i = 0; i<15;i++)
     X = all_types::_narrow(p->echoRef(p));
   output("test self reference constistency", X->echoLong(1743) == 1743);

   X = p->echoOtherAllTypes(X);
   output("test self reference typedef", X->echoLong(176767) == 176767);

   X = all_types::_narrow(p->echoObject(p->echoRef(p)));
   output("test object", X->echoLong(17123) == 17123);

   X = all_types::_narrow(p->echoOtherObject(p->echoRef(p)));
   output("test object typedef", X->echoLong(17123) == 17123);
 }
#endif

 // money
#ifdef MONEY
 {
   output("test fixed point", p->echoMoney("43.21") == "43.21");
 }
#endif

 // exeption
#ifdef EXCEPTION
 {
   bool Pass = 0;

   try 
     {
       p->testException (42, "answer");
     }
 
   catch (all_types::my_exception& ex) 
     {
       Pass = (ex.info == 2485);
#ifdef DEBUG
       cerr << "exception info: " << ex.info << endl;
#endif
       output("test user exception", Pass);
     }

   Pass = 0;

   try 
     {
       p->testUnknownException(1);
     }
   catch (CORBA::UNKNOWN &e) 
     {
       // Nothing
       Pass = 1;
     }
   output("test unknown exception", Pass);
 }
#endif

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

      all_types_var alltref = all_types::_narrow(obj);

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
  catch(...)
    {
      cerr << "Caught unknown error" << endl;
    }
  return 0;
}
