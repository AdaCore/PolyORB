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

   -- Inheritance from Vehicle
   ----------------------------
   type Ref is new Vehicle.Ref with null record ;
   type Ref_Ptr is access all Ref ;

   function To_Ref(The_Ref: in Corba.Object.Ref'Class) return Ref ;

   -- Inheritance from Weapon
   --------------------------
   -- Weapon Ref is the same object as Ref, but viewed as
   -- a descendant of Weapon.Ref
   -- To get such a view, use the following To_Ref function
   -- with an object of type Ref
   -- type Weapon_Ref is new Weapon.Ref with private ;
   -- type Weapon_Ref_Ptr is access all Weapon_Ref ;

   -- function To_Ref(The_Ref: in Corba.Object.Ref'Class) return Weapon_Ref ;

   subtype Name is Weapon.Name ;





   --------------------------------------------------
   ----    not in  spec AdaBroker specific       ----
   --------------------------------------------------

   function Get_Repository_Id(Self : in Ref) return Corba.String ;

   function Is_A(The_Ref: in Ref;
                 Repo_Id: in Corba.String)
                 return Corba.Boolean ;

   -- function Get_Repository_Id(Self : in Weapon_Ref) return Corba.String ;

   -- function Widen_From_The_Most_Derived_Intf(The_Ref: in Weapon_Ref;
   --                                          Repo_Id: in Corba.String)
   --                                          return Corba.Object'Class ;


private

   procedure Initialize (Self: in out Ref);

   Nil_Ref : aliased Ref := (Corba.Object.Nil_Ref with null record) ;


end Tank ;

