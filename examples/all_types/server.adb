----------------------------------------------------------------------------
----                                                                    ----
----     This in a hand-written server for All_Types example             ----
----                                                                    ----
----     It provides a declaration of each simple type with the         ----
----     echo function associated.                                      ----
----                                                                    ----
----                                                                    ----
----                server                                              ----
----                                                                    ----
----                authors : Fabien Azavant, Sebastien Ponce           ----
----                                                                    ----
----------------------------------------------------------------------------

with Corba ; use Corba ;
with Corba.Orb ; use Corba.Orb ;
with Corba.Boa ; use Corba.Boa ;
with Text_IO ; use Text_IO ;
with All_types.Impl ;

procedure server is
   Orb : Corba.Orb.Object := Corba.Orb.Orb_Init("omniORB2") ;
   Boa : Corba.Boa.Object := Corba.Orb.Boa_Init(Orb, "omniORB2_BOA") ;
   MyAll_Types : All_Types.Impl.Object ;
   Ior : Corba.String ;
begin
   Put_Line("main: starting server") ;

   Object_Is_Ready(Boa, MyAll_Types) ;
   Put_Line("main: Object is ready !") ;

   Ior := Object_To_String(MyAll_Types) ;
   Put_Line("'" & To_Standard_String(Ior) & "'") ;

   Implementation_Is_Ready(Boa) ;

   Put_Line("I Should not print that !!!") ;

end ;
