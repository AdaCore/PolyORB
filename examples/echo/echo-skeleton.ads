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


   procedure Dispatch (Myself : Omniobject.Implemented_Object_Ptr ;
                       Orls : in out Giop_S.Object ;
                       Orl_Op : in Standard.String ;
                       Orl_Response_Expected : in Corba.Boolean ;
                       Returns : out Corba.Boolean ) ;

end Echo.Skeleton ;
