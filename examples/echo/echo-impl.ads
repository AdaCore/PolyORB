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

with Giop_S, Corba.Object, OmniORB ;

package Echo.Impl is

   --------------------------------------------------
   ----                spec                      ----
   --------------------------------------------------

   type Object is new Corba.Object.Ref with private;

   function EchoString(Self : access Object;
                         Message : in Corba.String) return Corba.String ;


   --------------------------------------------------
   ----              not in  spec                ----
   --------------------------------------------------

   procedure Init (Self : in out Object; K : in OmniORB.ObjectKey);
   -- initializer

   function Dispatch (Self : in Object ;
                      Orls : in Giop_S.Object ;
                      Orl_Op : in Corba.String;
                      Orl_Response_Expected : Corba.Boolean)
                      return Corba.Boolean;
   -- called by the ORB's dispatch function in omniObject.ads
   -- calls the function whose name is Orl_OP
   -- returns true on success, false on failure


private

   type Object is new Corba.Object.Ref with null record;


End Echo.Impl ;










