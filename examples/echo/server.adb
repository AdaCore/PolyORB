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

with Corba, Corba.Orb, Corba.Boa ;
with Text_IO ;
with Echo.Impl ;

procedure server is
   Server : Echo.Impl.Object ;
begin

   -- ORB initialisation
   Corba.Orb.Init("omniORB2") ;

   -- BOA initialisation
   Corba.Boa.Init("omniORB2_BOA") ;

   -- telling the BOA there is a new object
   Corba.Boa.Object_Is_Ready(Server) ;

   -- getting this object's IOR
   Text_IO.Put_Line(Corba.To_Standard_String(Corba.Orb.Object_To_String(Server))) ;


   -- telling the BOA we are ready
   -- the BOA blocks indefinitely on this call
   Corba.Boa.Impl_Is_Ready(server) ;

end ;
