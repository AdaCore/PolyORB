----------------------------------------------------------------------------
----                                                                    ----
----     This in an example which is hand-written                       ----
----     for the Weapon object                                          ----
----                                                                    ----
----                package Weapon                                      ----
----                                                                    ----
----                author : Sebastien Ponce                            ----
----                                                                    ----
----------------------------------------------------------------------------

with Corba;
with Omniproxycalldesc;
with Giop_C;


package Weapon is

   --------------------------------------------------
   ----                spec                      ----
   --------------------------------------------------

   type Ref is new Corba.Object.Ref with null record ;

   type Name is new Corba.String ;

   function To_Ref(The_Ref: in Corba.Object.Ref'Class) return Ref ;

   procedure Shoot (Self: in Ref; Weapon_Name: in Name) ;


   --------------------------------------------------
   ----    not in  spec omniORB specific         ----
   --------------------------------------------------

   type OmniProxyCallDesc_Weapon is new OmniProxyCallDesc.Object with private ;

   function AlignedSize(Self: in OmniProxyCallDesc_Weapon;
                          Size_In: in Corba.Unsigned_Long)
                        return Corba.Unsigned_Long ;

   procedure MarshalArguments(Self: in OmniProxyCallDesc_Weapon;
                                Giop_Client: in out Giop_C.Object) ;

   procedure UnmarshalReturnedValues(Self: in OmniProxyCallDesc_Weapon;
                                       Giop_Client: in out Giop_C.Object) ;


   --------------------------------------------------
   ----    not in  spec AdaBroker specific       ----
   --------------------------------------------------

   procedure AdaBroker_Cast_To_Parent(Real_Object: in Ref;
                                      Result: out Corba.Object.Ref'Class) ;



private

   type OmniProxyCallDesc_Weapon is new OmniProxyCallDesc.Object with record
      Weapon_Name : Name ;
   end record ;


End Weapon;
