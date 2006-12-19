// Java DII 'echoULong' client, cf all_types.idl for more details.

public class DynClient
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

	boolean pass = true;

	// Call the `unsigned long echoULong (unsigned long arg)` method.

	for (int j = 0; j < 10000; j++)
	    {
		org.omg.CORBA.Request req = obj._request( "echoULong" );

		org.omg.CORBA.Any param1 = req.add_in_arg( );
		param1.insert_ulong( 123 );

		org.omg.CORBA.TypeCode tc_return = orb.get_primitive_tc( org.omg.CORBA.TCKind.tk_ulong );
		req.set_return_type( tc_return );

		req.invoke();

		java.lang.Exception exception = req.env().exception();
		
		if ( exception != null )
		    {
			System.out.println ("Got an exception !");
			return;
		    }

		org.omg.CORBA.Any result = req.return_value();
		
		pass = pass & (result.extract_ulong () == (long) 123);
	    }

       // Output the result.
       System.out.println (pass);
    }
}
