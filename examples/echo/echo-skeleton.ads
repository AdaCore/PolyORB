----------------------------------------------------------------------------
----                                                                    ----
----     This in an example which is hand-written                       ----
----     for the echo object                                            ----
----                                                                    ----
----                package echo_skeletons                              ----
----                                                                    ----
----                authors : Fabien Azavant, Sebastien Ponce           ----
----                                                                    ----
----------------------------------------------------------------------------

with Omniobject ;
with Giop_S ;

with Adabroker_Debug ;
pragma Elaborate(Adabroker_Debug) ;

package Echo.Skeleton is

   Echo_Skeleton : constant Boolean := Adabroker_Debug.Is_Active("echo.skeleton") ;


   --------------------------------------------------
   ----                spec                      ----
   --------------------------------------------------

   type Object is abstract new Omniobject.Implemented_Object with null record ;
   type Object_Ptr is access all Object'Class ;

   function EchoString(Self : access Object ;
                       Message : in Corba.String) return Corba.String is abstract ;

   function EchoLong(Self : access Object ;
                     Message : in Corba.Long) return Corba.Long is abstract ;


   procedure Initialize(Self : in out Object) ;
   procedure Adjust(Self: in out Object) ;
   procedure Finalize(Self : in out Object) ;

private

   procedure Dispatch (Self : access Object ;
                       Orls : in out Giop_S.Object ;
                       Orl_Op : in Standard.String ;
                       Orl_Response_Expected : in Corba.Boolean ;
                       Returns : out Corba.Boolean ) ;

end Echo.Skeleton ;
