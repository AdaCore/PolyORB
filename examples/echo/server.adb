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
with Text_IO ; use Text_Io ;

procedure server is
   Orb : Corba.Orb.Object  ;
   Boa : Corba.Boa.Object ;
begin
   Put_Line("starting server") ;

   Orb := Corba.Orb.Orb_Init("omniORB2") ;
   Put_Line("ORB initialized") ;

   Boa := Corba.Orb.Boa_Init("omniORB2_BOA") ;
   Put_Line("BOA initialized") ;

end ;
