with Corba ;
with Giop_C ;
with Omniproxycalldesc ;
with Chicken ;

package Egg.Proxies is

   type Hatch_Proxy is new OmniProxyCallDesc.Object with private ;

   procedure Init(Self : in out Hatch_Proxy) ;

   function Operation (Self : in Hatch_Proxy)
                       return CORBA.String ;

   function Align_Size(Self: in Hatch_Proxy ;
                         Size_In: in Corba.Unsigned_Long)
                         return Corba.Unsigned_Long ;

   procedure Marshal_Arguments(Self: in Hatch_Proxy ;
                               Giop_Client: in out Giop_C.Object) ;

   procedure Unmarshal_Returned_Values(Self: in out Hatch_Proxy ;
                                       Giop_Client: in out Giop_C.Object) ;

   function Get_Result (Self : in Hatch_Proxy)
                        return Chicken_Forward.Ref;


private

   type Hatch_Proxy is new OmniProxyCallDesc.Object with record
      Private_Result : Chicken.Most_Derived_Ref_Ptr := null ;
   end record ;

   procedure Finalize(Self : in out Hatch_Proxy) ;

end Egg.Proxies ;


