----------------------------------------------------------------------------
----                                                                    ----
----     This in an example which is hand-written                       ----
----     for the echo object (corresponds to eg2_clt.cc in omniORB      ----
----                                                                    ----
----                client                                              ----
----                                                                    ----
----                authors : Fabien Azavant, Sebastien Ponce           ----
----                                                                    ----
----------------------------------------------------------------------------


with Ada.Command_Line ;
with Text_Io ; use Text_Io ;
with Corba, Corba.Orb, Corba.Boa, Corba.Object ;
with Echo ;


procedure toto is

   Aaa : Corba.Object.Ref ;
   myecho : Echo.Ref ;

begin

   Put_Line("main : Starting client") ;

   Myecho := (Aaa with null record) ;

   Put_Line("ok") ;

end toto ;




