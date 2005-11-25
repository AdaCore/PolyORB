// Generic DSI Server.

public class DynServer
{
    public static void main( String args[] )
    {
        org.omg.CORBA.ORB orb = org.omg.CORBA.ORB.init( args, null );

        org.omg.CORBA.Object objPoa = null;
        org.omg.PortableServer.POA rootPOA = null;

        try
        {
            objPoa = orb.resolve_initial_references( "RootPOA" );
        }
        catch ( org.omg.CORBA.ORBPackage.InvalidName ex )
        {
            System.out.println( "Couldn't find RootPOA!" );
            System.exit( 1 );
        }

        rootPOA = org.omg.PortableServer.POAHelper.narrow( objPoa );

        DynSkeleton skeleton = new DynSkeleton( orb );

        byte[] servantId = null ;

        try
        {
            servantId = rootPOA.activate_object( skeleton );
        }
        catch ( org.omg.PortableServer.POAPackage.WrongPolicy ex )
        {
            ex.printStackTrace();
        }
        catch ( org.omg.PortableServer.POAPackage.ServantAlreadyActive ex )
        {
            ex.printStackTrace();
        }

        org.omg.CORBA.Object obj = null ;

        try
        {
            obj = rootPOA.id_to_reference( servantId );
        }
        catch ( org.omg.PortableServer.POAPackage.WrongPolicy ex )
        {
            ex.printStackTrace();
        }
        catch ( org.omg.PortableServer.POAPackage.ObjectNotActive ex )
        {
            ex.printStackTrace();
        }

        String reference = orb.object_to_string( obj );

        try
        {
            java.io.FileOutputStream file = new java.io.FileOutputStream( "IOR" );
            java.io.PrintStream pfile = new java.io.PrintStream( file );
            pfile.println( reference );
            file.close();

	    System.out.println (reference);

        }
        catch ( java.io.IOException ex )
        {
            System.out.println( "File error" );
        }


        try
        {
            rootPOA.the_POAManager().activate();

            System.out.println( "The server is ready..." );

            orb.run();
        }
        catch ( Exception ex )
        {
            ex.printStackTrace();
        }
    }
}

