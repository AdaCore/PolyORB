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

with Omniorb ;
with Giop_S ;

with Echo.Impl ;

package Echo.Skeleton is


   procedure AdaBroker_Dispatch (Self : in out Echo.Impl.Object ;
                                 Orls : in Giop_S.Object ;
                                 Orl_Op : in Corba.String ;
                                 Orl_Response_Expected : in Corba.Boolean ;
                                 Returns : out Corba.Boolean ) ;

end Echo.Skeleton ;
