----------------------------------------------------------------------------
----                                                                    ----
----     This in an example which is hand-written                       ----
----     for the echo object                                            ----
----                                                                    ----
----                package echo                                        ----
----                                                                    ----
----                authors : Fabien Azavant, Sebastien Ponce           ----
----                                                                    ----
----------------------------------------------------------------------------

with Corba, Corba.Object ;
with Omniproxycalldesc ;
with Giop_C ;

package Echo is

   --------------------------------------------------
   ----                spec                      ----
   --------------------------------------------------

   type Abstract_Echo is abstract new Corba.Object.Ref with null record ;

   type Ref is new Abstract_Echo with null record ;

   -- function To_Ref(The_Ref: in Corba.Object.Ref'Class) return Ref ;
   -- inherited from Corba.Object.Ref

   function EchoString(Self: in Ref ;
                       Message: in Corba.String) return Corba.String ;


   --------------------------------------------------
   ----    not in  spec AdaBroker specific       ----
   --------------------------------------------------

   procedure AdaBroker_Cast_To_Parent(Real_Object: in Ref;
                                      Result: out Corba.Object.Ref'Class) ;




End Echo ;




