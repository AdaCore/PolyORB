with Giop_C ;
with Omniproxycalldesc ;
with Rope ;
with Iop ;
with Corba ;
with weapon ;
package tank.Proxies is 
   -----------------------------------------------------------
   ---               move
   -----------------------------------------------------------

   type move_Proxy is new OmniProxyCallDesc.Object with private ;

   procedure Init(Self : in out move_Proxy ;
                  fast : in Corba.String) ;

   function Operation(Self : in move_Proxy )
                      return Corba.String ;

   function Align_Size(Self : in move_Proxy ;
                       Size_In : in Corba.Unsigned_Long)
                       return Corba.Unsigned_Long ;

   procedure Marshal_Arguments(Self : in move_Proxy ;
                               Giop_Client : in out Giop_C.Object) ;

   procedure Unmarshal_Returned_Values(Self : in out move_Proxy ;
                                       Giop_Client : in out Giop_C.Object) ;

   function Get_Result (Self : in move_Proxy)
                        return Corba.String; 


   -----------------------------
   -- inheritance from weapon
   -----------------------------

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
   type move_Proxy is new OmniProxyCallDesc.Object with record 
      fast : Corba.String_Ptr := null ;
      Private_Result : Corba.String_Ptr := null;
   end record; 
   procedure Finalize(Self : in out move_Proxy) ;

   type shoot_Proxy is new OmniProxyCallDesc.Object with record 
      ranges : Corba.Long_Ptr := null ;
   end record; 
   procedure Finalize(Self : in out shoot_Proxy) ;

end tank.Proxies ;
