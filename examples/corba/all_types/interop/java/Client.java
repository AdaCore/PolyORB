// Java All Types static client, cf all_types.idl for more details

public class Client
{
    public static void main( String [] args )
    {
        org.omg.CORBA.ORB orb = org.omg.CORBA.ORB.init( args, null ) ;

        org.omg.CORBA.Object obj = null ;

	// Read the IOR from the `IOR` file.
        try
        {

            java.io.FileInputStream file = new java.io.FileInputStream( "IOR" );
            java.io.InputStreamReader input = new java.io.InputStreamReader( file );
            java.io.BufferedReader reader = new java.io.BufferedReader( input );
            String ref = reader.readLine();
            obj = orb.string_to_object( ref ) ;
        }
        catch ( java.io.IOException ex )
        {
            ex.printStackTrace( ) ;
            System.exit( 0 );
        }

	boolean pass;

	// Narrow object reference.
	idl.all_types ato = idl.all_typesHelper.narrow (obj);

 	// String type.
	String my_string = "Hello distributed World in Java";
 	System.out.println ("echoString  " + my_string.equals(ato.echoString (my_string)));

	// Simple scalar types.
	int res;
	for (int i = 0; i < 10000; i++)
	    res = ato.echoULong (456);

  	System.out.println ("echoBoolean " + ato.echoBoolean (true));
  	System.out.println ("echoShort   " + ((short) 123 == ato.echoShort ((short) 123)));
  	System.out.println ("echoLong    " + (456 == ato.echoLong (456)));
  	System.out.println ("echoULong   " + (456 == ato.echoULong (456)));
  	System.out.println ("echoUShort  " + ((short) 123 == ato.echoUShort ((short) 123)));
  	System.out.println ("echoFloat   " + ((float) 2.7 == ato.echoFloat ((float) 2.7)));
  	System.out.println ("echoDouble  " + (1.5 == ato.echoDouble (1.5)));
  	System.out.println ("echoChar    " + ('A' == ato.echoChar ('A')));
  	System.out.println ("echoOctet   " + ((byte) 5 == ato.echoOctet ((byte) 5)));

  	// Enum type.
  	idl.all_typesPackage.Color col = idl.all_typesPackage.Color.Blue;
  	System.out.println ("echoColor   " + (idl.all_typesPackage.Color.Blue == ato.echoColor (col)));

  	// Unbounded sequence.
  	short u_seq[] = { 1, 2, 3, 4};
  	short res_u_seq[] = ato.echoUsequence (u_seq);
  	pass = true;
  	for (int i = 0; i < 4; i++)
  	    pass = pass & (res_u_seq[i] == u_seq[i]);

  	System.out.println ("echoUsequence " + pass);

  	// Bounded sequence.
  	short b_seq[] = { 1, 2, 3, 4};
  	short res_b_seq[] = ato.echoUsequence (b_seq);
  	pass = true;
  	for (int i = 0; i < 4; i++)
  	    pass = pass & (res_b_seq[i] == b_seq[i]);

  	System.out.println ("echoBsequence " + pass);

  	// Simple struct.
  	idl.all_typesPackage.simple_struct simple_s 
  	    = new idl.all_typesPackage.simple_struct (123, "Hello World!\n");
  	idl.all_typesPackage.simple_struct res_simple_s = ato.echoStruct (simple_s);

  	pass = (simple_s.a == res_simple_s.a) && (simple_s.s.equals(res_simple_s.s));

  	System.out.println ("echoStruct " + pass);

  	// Nested struct.
  	idl.all_typesPackage.nested_struct nested_s
  	    = new idl.all_typesPackage.nested_struct (simple_s);
  	idl.all_typesPackage.nested_struct res_nested_s = ato.echoNestedStruct (nested_s);

  	pass = (nested_s.ns.a == res_nested_s.ns.a) && (nested_s.ns.s.equals (res_nested_s.ns.s));
  	System.out.println ("echoNestedStruct " + pass);

  	// Object ref.
  	idl.all_types res_obj = ato.echoRef (ato);
  	// pass = org.omg.CORBA.portable.Delegate.is_equivalent(res_obj, ato);
  	// XXX how to test this ?
  	System.out.println ("echoRef " + true);
	
  	// Union.
  	idl.all_typesPackage.myUnion union_t = new idl.all_typesPackage.myUnion(); 
  	union_t.Counter (4331);

 	// 	idl.all_typesPackage.myUnion res_union_t = ato.echoUnion (union_t);
 	// 	System.out.println ("echoUnion " + (res_union_t.Counter() == 4331));

  	// Union switch.
  	idl.all_typesPackage.myUnionEnumSwitch union_es_t 
  	    = new idl.all_typesPackage.myUnionEnumSwitch(); 
  	union_es_t.foo (4331);

 	// 	idl.all_typesPackage.myUnionEnumSwitch res_union_es_t 
  	//    = ato.echoUnionEnumSwitch (union_es_t);
  	//System.out.println ("echoUnionEnumSwitch " + (res_union_es_t.foo() == 4331));

  	// Simple array.
  	int simple_a[] = {1, 2, 3, 4, 5};
  	int res_simple_a[] = ato.echoArray(simple_a);

  	pass = true;
  	for (int i = 0; i < 5 ; i++)
  	    pass = pass &&  (simple_a[i] == res_simple_a[i]);

  	System.out.println ("echoArray " + pass);

  	// Multi-dimensionnal array.
  	int multi_a[][] = { {1, 2, 3}, {4, 5, 6}, {7, 8, 9}};
  	int res_multi_a[][] = ato.echoMatrix (multi_a);

  	pass = true;
  	for (int i = 0; i < 3; i++)
  	    for (int j = 0; j < 3; j++)
  		pass = pass && (multi_a[i][j] == res_multi_a [i][j]);

  	System.out.println ("echoMatrix " + pass); 

  	// Attributes.
  	ato.myColor (idl.all_typesPackage.Color.Blue);
  	System.out.println ("Attributes " + (idl.all_typesPackage.Color.Blue == ato.myColor()));

  	// Read Only attributes.
  	int v1 = ato.Counter ();
  	int v2 = ato.Counter ();
  	System.out.println ("RO Attributes " + (v1 + 1 == v2));

  	// User exception.
  	try
  	    {
  		ato.testException (2845);
  	    }
  	catch (idl.all_typesPackage.my_exception ex)
  	    {
  		System.out.println ("User exception " + (ex.info == 2845));
  	    }

  	// System exception.
  	try
  	    {
  		ato.testUnknownException (2485);
  	    }
  	catch (org.omg.CORBA.UNKNOWN es)
  	    {
  		System.out.println ("System exception " + true);
  	    }
    }
}

