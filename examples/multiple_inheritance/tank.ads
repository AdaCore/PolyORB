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
   type Ref is new Vehicle.Ref with private;
   type Ref_Ptr is access all Ref ;

   function To_Ref(The_Ref: in Corba.Object.Ref'Class) return Ref ;

   -- Inheritance from Weapon
   --------------------------
   -- Weapon Ref is the same object as Ref, but viewed as
   -- a descendant of Weapon.Ref
   -- To get such a view, use the following To_Ref function
   -- with an object of type Ref
   type Weapon_Ref is new Weapon.Ref with private ;
   type Weapon_Ref_Ptr is access all Weapon_Ref ;

   function To_Ref(The_Ref: in Corba.Object.Ref'Class) return Weapon_Ref ;

   subtype Name is Weapon.Name ;





   --------------------------------------------------
   ----    not in  spec AdaBroker specific       ----
   --------------------------------------------------

   procedure AdaBroker_Cast_To_Parent(Real_Ref: in Ref;
                                      Result: out Corba.Object.Ref'Class) ;

   function Get_Dynamic_Ref(Self: in Ref) return Corba.Object.Ref'Class ;
   function Get_Dynamic_Ref(Self: in Weapon_Ref) return Corba.Object.Ref'Class ;


private


   type Ref is new Vehicle.Ref with record
      AdaBroker_Weapon : Weapon_Ref ;
   end record;

   type Weapon_Ref is new Weapon.Ref with record
      Dynamic_Ref : Ref_Ptr := null ;
      Must_Be_Freed : Boolean := False ;
   end record ;

   procedure Initialize(Object : in out Ref) ;
   procedure Adjust(Object : in out Ref) renames Initialize ;

   procedure Adjust(Object : in out Weapon_Ref) ;
   procedure Finalize(Object : in out Weapon_Ref) ;

end Tank ;

