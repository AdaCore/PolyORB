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

with Corba ;

package Echo.Impl is

   --------------------------------------------------
   ----                spec                      ----
   --------------------------------------------------

   type Object is new Echo.Abstract_Echo with private;

   function EchoString(Self : access Object;
                       Message : in Corba.String) return Corba.String ;



private

   type Object is new Echo.Abstract_Echo with null record;


End Echo.Impl ;










