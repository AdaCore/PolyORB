----------------------------------------------------------------------------
----                                                                    ----
----     This in an example which is hand-written                       ----
----     for the echo object (corresponds to eg2_impl.cc in omniORB     ----
----                                                                    ----
----                server                                              ----
----                                                                    ----
----                authors : Fabien Azavant, Sebastien Ponce           ----
----                                                                    ----
----------------------------------------------------------------------------

with Corba ; use Corba ;
with Corba.Orb ; use Corba.Orb ;
with Corba.Boa ; use Corba.Boa ;
with Text_IO ; use Text_Io ;
with Echo.Impl ;

procedure server is
   Orb : Corba.Orb.Object := Corba.Orb.Orb_Init("omniORB2") ;
   Boa : Corba.Boa.Object := Corba.Orb.Boa_Init(Orb, "omniORB2_BOA") ;
   Myecho : Echo.Impl.Object ;
   Ior : Corba.String ;
begin
   Put_Line("main: starting server") ;

   Object_Is_Ready(Boa, Myecho) ;
   Put_Line("main: Object is ready !") ;

   Ior := Object_To_String(Myecho) ;
   Put_Line("'" & To_Standard_String(Ior) & "'") ;

   Implementation_Is_Ready(Boa) ;

   Put_Line("I Should not print that !!!") ;

end ;
