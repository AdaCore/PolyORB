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

with Omniobject ;

package Echo.Impl is

   --------------------------------------------------
   ----                spec                      ----
   --------------------------------------------------

   type Object is new Omniobject.Implemented_Object with private ;
   type Object_Ptr is access all Object'Class ;

   function EchoString(Self : access Object;
                       Message : in Corba.String) return Corba.String ;



private

   type Object is new Omniobject.Implemented_Object with null record ;


End Echo.Impl ;










