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

with Echo.Skeleton ;

with Adabroker_Debug ;
pragma Elaborate(Adabroker_Debug) ;

package Echo.Impl is

   Echo_Impl : constant Boolean := Adabroker_Debug.Is_Active("echo.impl") ;

   --------------------------------------------------
   ----                spec                      ----
   --------------------------------------------------

   type Object is new Echo.Skeleton.Object with private ;
   type Object_Ptr is access all Object'Class ;

   function EchoString(Self : access Object;
                       Message : in Corba.String) return Corba.String ;

   function EchoLong(Self : access Object ;
                     Message : in Corba.Long) return Corba.Long ;

private

   -- you may add fields to this record
   type Object is new Echo.Skeleton.Object with record
      null ;
   end record ;

End Echo.Impl ;
