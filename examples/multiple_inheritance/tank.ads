----------------------------------------------------------------------------
----                                                                    ----
----     This in an example which is hand-written                       ----
----     for the Tank object                                            ----
----                                                                    ----
----                package Tank                                        ----
----                                                                    ----
----     author : Sebastien Ponce, Fabien Azavant                       ----
----                                                                    ----
----------------------------------------------------------------------------

with Corba, Corba.Object ;
with Vehicle, Weapon;


package Tank is

   --------------------------------------------------
   ----                spec                      ----
   --------------------------------------------------

   -- Inheritance from vehicle
   type Ref is new Vehicle.Ref with private;
   type Ref_Ptr is access all Ref ;

   -- Added from weapon for multiple inheritance
   subtype Name is Weapon.Name ;

   function To_Ref(The_Ref: in Corba.Object.Ref'Class) return Ref ;



   --------------------------------------------------
   ----    not in  spec AdaBroker specific       ----
   --------------------------------------------------

   procedure AdaBroker_Cast_To_Parent(Real_Object: in Ref;
                                      Result: out Corba.Object.Ref'Class) ;


private

   type Ref is new Vehicle.Ref with record
      AdaBroker_Weapon : Weapon.Ref_Ptr ;
   end record;

end Tank ;

