with Giop_C ;
with Omniproxycalldesc ;
with Rope ;
with Iop ;
package weapon.Proxies is 
   -----------------------------------------------------------
   ---               shoot
   -----------------------------------------------------------

   type shoot_Proxy is new OmniProxyCallDesc.Object with private ;

   procedure Init(Self : in out shoot_Proxy ;
                  ranges : in Corba.Long) ;

   function Operation(Self : in shoot_Proxy )
                      return Corba.String ;

   function Align_Size(Self : in shoot_Proxy ;
                       Size_In : in Corba.Unsigned_Long)
                       return Corba.Unsigned_Long ;

   procedure Marshal_Arguments(Self : in shoot_Proxy ;
                               Giop_Client : in out Giop_C.Object) ;

private 
   type shoot_Proxy is new OmniProxyCallDesc.Object with record 
      ranges : Corba.Long_Ptr := null ;
   end record; 
   procedure Finalize(Self : in out shoot_Proxy) ;

end weapon.Proxies ;
