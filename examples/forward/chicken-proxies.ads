with Corba ;
with Giop_C ;
with Omniproxycalldesc ;
with Egg ;

package Chicken.Proxies is

   type Lay_Proxy is new OmniProxyCallDesc.Object with private ;

   procedure Init(Self : in out Lay_Proxy) ;

   function Operation (Self : in Lay_Proxy)
                       return CORBA.String ;

   function Align_Size(Self: in Lay_Proxy ;
                         Size_In: in Corba.Unsigned_Long)
                         return Corba.Unsigned_Long ;

   procedure Marshal_Arguments(Self: in Lay_Proxy ;
                               Giop_Client: in out Giop_C.Object) ;

   procedure Unmarshal_Returned_Values(Self: in out Lay_Proxy ;
                                       Giop_Client: in out Giop_C.Object) ;

   function Get_Result (Self : in Lay_Proxy)
                        return Egg_Forward.Ref;


private

   type Lay_Proxy is new OmniProxyCallDesc.Object with record
      Private_Result : Egg.Most_Derived_Ref_Ptr := null ;
   end record ;

   procedure Finalize(Self : in out Lay_Proxy) ;

end Chicken.Proxies ;


