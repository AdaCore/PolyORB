----------------------------------------------------------------------------
----                                                                    ----
----     This in an example which is hand-written                       ----
----     for the Weapon object                                          ----
----                                                                    ----
----                package Waepon_impl                                 ----
----                                                                    ----
----                author : Sebastien Ponce                            ----
----                                                                    ----
----------------------------------------------------------------------------

with Giop_S, Corba.Object, OmniORB ;

package Weapon.Impl is

   --------------------------------------------------
   ----                spec                      ----
   --------------------------------------------------

   type Object is new Corba.Implementation_Defined.Object with private;

   procedure Shoot (Self: in access Object; Weapon_Name: in Name) ;


   --------------------------------------------------
   ----              not in  spec                ----
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

   type Object is new Corba.Object.Ref with null record;

End Weapon.Impl ;
