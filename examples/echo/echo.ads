----------------------------------------------------------------------------
----                                                                    ----
----     This in an example which is hand-written                       ----
----     for the echo object                                            ----
----                                                                    ----
----                package echo                                        ----
----                                                                    ----
----                authors : Fabien Azavant, Sebastien Ponce           ----
----                                                                    ----
----------------------------------------------------------------------------

with Corba, Corba.Object ;
with Omniproxycalldesc;
with Giop_C;


package Echo is

   --------------------------------------------------
   ----                spec                      ----
   --------------------------------------------------

   type Ref is new Corba.Object.Ref with null record ;

   function To_Ref(The_Ref: in Corba.Object.Ref'Class) return Ref ;

   function EchoString(Self: in Ref; Message: in Corba.String) return Corba.String ;


   --------------------------------------------------
   ----    not in  spec omniORB specific         ----
   --------------------------------------------------

   type OmniProxyCallDesc_Echo is new OmniProxyCallDesc.Object with private ;

   function AlignedSize(Self: in OmniProxyCallDesc_Echo;
                          Size_In: in Corba.Unsigned_Long)
                        return Corba.Unsigned_Long ;

   procedure MarshalArguments(Self: in OmniProxyCallDesc_Echo;
                                Giop_Client: in out Giop_C.Object) ;

   procedure UnmarshalReturnedValues(Self: in OmniProxyCallDesc_Echo;
                                       Giop_Client: in out Giop_C.Object) ;

   function Result (Self : in Ref) return CORBA.String;

   --------------------------------------------------
   ----    not in  spec AdaBroker specific       ----
   --------------------------------------------------

   procedure AdaBroker_Cast_To_Parent(Real_Object: in Ref;
                                      Result: out Corba.Object.Ref'Class) ;



private

   type OmniProxyCallDesc_Echo is new OmniProxyCallDesc.Object with record
      Arg : Corba.String ;
      Private_Result : Corba.String ;
   end record ;


End Echo ;




