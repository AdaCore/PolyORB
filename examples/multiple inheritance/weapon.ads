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


package Waepon is

   --------------------------------------------------
   ----                spec                      ----
   --------------------------------------------------

   type Ref is new Corba.Object.Ref with null record ;

   type Name is new Corba.String ;

   function To_Ref(From: in Corba.Object.Ref'Class) return Ref ;

   procedure Shoot (Self: in Ref; Weapon_Name: in Name) ;


   --------------------------------------------------
   ----    not in  spec omniORB specific         ----
   --------------------------------------------------

   type OmniProxyCallDesc_Weapon is new OmniProxyCallDesc with private ;

   function AlignedSize(Self: in OmniProxyCallDesc_Weapon;
                          Size_In: in Corba.Unsigned_Long)
                        return Corba.Unsigned_Long ;

   procedure MarshalArguments(Self: in OmniProxyCallDesc_Weapon;
                                Giop_Client: in out Giop_C) ;

   procedure UnmarshalReturnedValues(Self: in OmniProxyCallDesc_Weapon;
                                       Giop_Client: in out Giop_C) ;


   --------------------------------------------------
   ----    not in  spec AdaBroker specific       ----
   --------------------------------------------------

   procedure AdaBroker_Cast_To_Parent(Real_Object: in Ref;
                                      Result: out Corba.Object'Class) ;



private

   type OmniProxyCallDesc_Weapon is new OmniProxyCallDesc with record
      Weapon_Name : Name ;
   end record ;


End Echo ;

