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

   type Object is new Corba.Implementation_Defined.Ref with private;

   function EchoString(Self : in access Object,
                         Message : in Corba.String) return Corba.String ;


   --------------------------------------------------
   ----              not in  spec                ----
   --------------------------------------------------

   procedure Init (Self : in out Object, K : in OmniORB.ObjectKey);
   -- initializer


private

   type Object is new Corba.Object.Ref with private;


End Echo_impl ;



