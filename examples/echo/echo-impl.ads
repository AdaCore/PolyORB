----------------------------------------------------------------------------
----                                                                    ----
----     This in an example which is hand-written                       ----
----     for the echo object                                            ----
----                                                                    ----
----                package echo_impl                                   ----
----                                                                    ----
----                authors : Fabien Azavant, Sebastien Ponce           ----
----                                                                    ----
----------------------------------------------------------------------------

package Echo.Impl is

   --------------------------------------------------
   ----                spec                      ----
   --------------------------------------------------

   type Object is new Corba.Object.Object with private ;

   function EchoString(Self : access Object;
                       Message : in Corba.String) return Corba.String ;



private


   type Object is new Corba.Object.Object with null record ;


End Echo.Impl ;










