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


package Tank is

   --------------------------------------------------
   ----                spec                      ----
   --------------------------------------------------

   -- Inheritance from vehicle
   type Ref is new Vehicle with null record;

   function To_Ref(From: in Corba.Object.Ref'Class) return Ref ;


   -- Added from weapon for multiple inheritance
   type Name is new Corba.String ;

   procedure Shoot (Self: in Ref; Weapon_Name: in Name) ;



   --------------------------------------------------
   ----    not in  spec omniORB specific         ----
   --------------------------------------------------

   type OmniProxyCallDesc_Tank is new OmniProxyCallDesc with private ;

   function AlignedSize(Self: in OmniProxyCallDesc_Tank;
                          Size_In: in Corba.Unsigned_Long)
                        return Corba.Unsigned_Long ;

   procedure MarshalArguments(Self: in OmniProxyCallDesc_Tank;
                                Giop_Client: in out Giop_C) ;

   procedure UnmarshalReturnedValues(Self: in OmniProxyCallDesc_Tank;
                                       Giop_Client: in out Giop_C) ;

   --------------------------------------------------
   ----    not in  spec AdaBroker specific       ----
   --------------------------------------------------

   procedure AdaBroker_Cast_To_Parent(Real_Object: in Ref;
                                      Result: out Corba.Object'Class) ;

   function AdaBroker_To_Vehicle (self: in Ref) return Vehicule.Ref ;

   function AdaBroker_To_Weapon (self: in Ref) return Weapon.Ref ;



private

   type OmniProxyCallDesc_Tank is new OmniProxyCallDesc_Vehicle with record
      Weapon_Name : Name ;
   end record;


End Echo ;

