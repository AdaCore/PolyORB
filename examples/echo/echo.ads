----------------------------------------------------------------------------
----                                                                    ----
----     This in an example which is hand-written                       ----
----     for the echo object (corresponds to eg2_clt.cc in omniORB      ----
----                                                                    ----
----                package echo                                        ----
----                                                                    ----
----------------------------------------------------------------------------


package Echo is

   type Ref is new Corba.Object.Ref with null record ;

   function To_Ref(From: in Corba.Object.Ref'Class) return Ref ;

   function EchoString(S: in Corba.String) return Corba.String ;

End Echo ;
