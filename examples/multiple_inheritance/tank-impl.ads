----------------------------------------------------------------------------
----                                                                    ----
----     This in an example which is hand-written                       ----
----     for the Tank object                                            ----
----                                                                    ----
----                package Tank.Impl                                   ----
----                                                                    ----
----                author : Sebastien Ponce                            ----
----                                                                    ----
----------------------------------------------------------------------------

with Giop_S, Corba.Object, OmniORB ;

package Tank.Impl is

   --------------------------------------------------
   ----                spec                      ----
   --------------------------------------------------

   -- Inheritance from vehicle
   type Object is new Vehicle.Impl.Object with private;


   -- Added from weapon for multiple inheritance
   type Name is new Corba.String ;

   procedure Shoot (Self: in access Object ;
                    Weapon_Name: in Name) ;



   --------------------------------------------------
   ----    not in  spec omniORB specific         ----
   --------------------------------------------------

   procedure Init (Self : in out Object; K : in OmniORB.ObjectKey);
   -- initializer

   function Dispatch (Self : in Object;
                      Orls : in out Giop_S;
                      Orl_Op : in Corba.String;
                      Orl_Response_Expected : in Corba.Boolean)
                      return Corba.Boolean;
   -- called by the ORB's dispatch function in omniObject.ads
   -- calls the function whose name is Orl_OP
   -- returns true on success, false on failure


private

   type Object is new Vehicle.Impl.Object with null record;

End Tank.Impl ;
