// DSI Server that supports the `unsigned long echoULong (unsigned long var)` method.

public class DynSkeleton
    extends org.omg.PortableServer.DynamicImplementation
{
    public String[ ] _all_interfaces( org.omg.PortableServer.POA poa, byte [] objectId )
    {
        String[] _ids_list = { "IDL:all_types:1.0" };
        return _ids_list;
    }

    private org.omg.CORBA.ORB m_orb;

    public DynSkeleton( org.omg.CORBA.ORB orb )
    {
        m_orb = orb;
    }

    public void invoke ( org.omg.CORBA.ServerRequest request )
    {
        String operation = request.operation();

        if ( operation.equals( "echoULong" ) )
        {
            org.omg.CORBA.NVList argList = m_orb.create_list( 0 );

            org.omg.CORBA.Any arg0 = m_orb.create_any();
            arg0.type( m_orb.get_primitive_tc( org.omg.CORBA.TCKind.tk_ulong ) );
            argList.add_value( "", arg0, org.omg.CORBA.ARG_IN.value );

            request.arguments( argList );

	    request.set_result( arg0 );

        }
        else
            throw new org.omg.CORBA.BAD_OPERATION();
    }
}

