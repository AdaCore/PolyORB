----------------------------------------------------------------------------
----                                                                    ----
----     This in an example which is hand-written                       ----
----     for the Vehicle object                                         ----
----                                                                    ----
----                package Vehicle                                     ----
----                                                                    ----
----                authors : Sebastien Ponce                           ----
----                                                                    ----
----------------------------------------------------------------------------

with Corba;
with Omniproxycalldesc;
with Giop_C;


package Vehicle is

   --------------------------------------------------
   ----                spec                      ----
   --------------------------------------------------

   type Ref is new Corba.Object.Ref with null record;

   function To_Ref(The_Ref: in Corba.Object.Ref'Class) return Ref ;

   procedure Set_Mark (Self: in Ref ;
                       To: in Corba.String) ;

   function Get_Mark (Self: in Ref) return Corba.String ;

   procedure Drive (Self: in Ref; Vehicle_Mark: in Corba.String) ;


   --------------------------------------------------
   ----    not in  spec omniORB specific         ----
   --------------------------------------------------

   type OmniProxyCallDesc_Vehicle is new OmniProxyCallDesc with private ;

   function AlignedSize(Self: in OmniProxyCallDesc_Vehicle;
                          Size_In: in Corba.Unsigned_Long)
                        return Corba.Unsigned_Long ;

   procedure MarshalArguments(Self: in OmniProxyCallDesc_Vehicle;
                                Giop_Client: in out Giop_C.Object) ;

   procedure UnmarshalReturnedValues(Self: in OmniProxyCallDesc_Vehicle;
                                       Giop_Client: in out Giop_C.Object) ;

   function Result (Self : in Ref) return CORBA.String;


   --------------------------------------------------
   ----    not in  spec AdaBroker specific       ----
   --------------------------------------------------

   procedure AdaBroker_Cast_To_Parent(Real_Object: in Ref;
                                      Result: out Corba.Object.Ref'Class) ;



private

   type OmniProxyCallDesc_Vehicle is new OmniProxyCallDesc.Object with record
      Vehicle_Mark : Corba.String ;
      Private_Result : Corba.String ;
   end record ;


End Vehicle ;




