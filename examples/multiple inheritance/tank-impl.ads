----------------------------------------------------------------------------
----                                                                    ----
----     This in an example which is hand-written                       ----
----     for the Tank object                                            ----
----                                                                    ----
----                package Tank                                        ----
----                                                                    ----
----                author : Sebastien Ponce                            ----
----                                                                    ----
----------------------------------------------------------------------------

with Corba;
with Omniproxycalldesc;
with Giop_C;


package Tank.Impl is

   --------------------------------------------------
   ----                spec                      ----
   --------------------------------------------------

   -- Inheritance from vehicle
   type Object is new Tank.Impl.Object with private;


   -- Added from weapon for multiple inheritance
   type Name is new Corba.String ;

   procedure Shoot (Self: in Ref;
                    Weapon_Name: in Name) ;



   --------------------------------------------------
   ----    not in  spec omniORB specific         ----
   --------------------------------------------------

   procedure Init (Self : in out Object; K : in OmniORB.ObjectKey);
   -- initializer

   function Dispatch (Self : in Object;
                      Orls : in out Giop_S;
                      Orl_Op : in Corba.String;
                      Orl_Response_Expected : Corba.Boolean)
                      return Corba.Boolean;
   -- called by the ORB's dispatch function in omniObject.ads
   -- calls the function whose name is Orl_OP
   -- returns true on success, false on failure


private

   type Object is new Tank.Impl.Object with null record;

End Echo ;
