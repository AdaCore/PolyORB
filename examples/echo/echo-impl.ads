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
with Giop_S ;

with Adabroker_Debug ;
pragma Elaborate(Adabroker_Debug) ;

package Echo.Impl is

   Echo_Impl : constant Boolean := Adabroker_Debug.Is_Active("echo.impl") ;

   --------------------------------------------------
   ----                spec                      ----
   --------------------------------------------------

   type Object is new Omniobject.Implemented_Object with private ;
   type Object_Ptr is access all Object'Class ;

   function EchoString(Self : access Object;
                       Message : in Corba.String) return Corba.String ;



   procedure Dispatch (Self : in out Echo.Impl.Object ;
                       Orls : in out Giop_S.Object ;
                       Orl_Op : in Standard.String ;
                       Orl_Response_Expected : in Corba.Boolean ;
                       Returns : out Corba.Boolean ) ;

private

   type Object is new Omniobject.Implemented_Object with null record ;

   procedure Initialize(Self : in out Object) ;

End Echo.Impl ;










