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

with Corba, Omniproxycalldesc, Giop_C, Vehicle, Weapon;


package Tank is

   --------------------------------------------------
   ----                spec                      ----
   --------------------------------------------------

   -- Inheritance from vehicle
   type Ref is new Vehicle.Ref with private;

   -- Added from weapon for multiple inheritance
   subtype Name is new Weapon.Name ;

   procedure Shoot (Self: in Ref; Weapon_Name: in Name) ;



   --------------------------------------------------
   ----    not in  spec omniORB specific         ----
   --------------------------------------------------

   type OmniProxyCallDesc_Tank is new Vehicle.OmniProxyCallDesc_Vehicle
     with private ;

   function AlignedSize(Self: in OmniProxyCallDesc_Tank;
                          Size_In: in Corba.Unsigned_Long)
                        return Corba.Unsigned_Long ;

   procedure MarshalArguments(Self: in OmniProxyCallDesc_Tank;
                                Giop_Client: in out Giop_C.Object) ;

   procedure UnmarshalReturnedValues(Self: in OmniProxyCallDesc_Tank;
                                       Giop_Client: in out Giop_C.Object) ;

   --------------------------------------------------
   ----    not in  spec AdaBroker specific       ----
   --------------------------------------------------

   procedure AdaBroker_Cast_To_Parent(Real_Object: in Ref;
                                      Result: out Corba.Object.Ref'Class) ;

   function AdaBroker_To_Vehicle (Self: in Ref) return Vehicule.Ref ;

   function AdaBroker_To_Weapon (Self: in Ref) return Weapon.Ref ;



private

   type OmniProxyCallDesc_Tank is new OmniProxyCallDesc_Vehicle with record
      Weapon_Name : Name ;
   end record;

   type Ref is new Vehicle.Ref with record
      AdaBroker_pere2 : access Weapon.Ref;
   end record;

end Tank ;

