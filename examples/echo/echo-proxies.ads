with Giop_C ;
with Omniproxycalldesc ;
with Rope ;
with Iop ;
with Corba ;
package Echo.Proxies is 
   -----------------------------------------------------------
   ---               echoString
   -----------------------------------------------------------

   type echoString_Proxy is new OmniProxyCallDesc.Object with private ;

   procedure Init(Self : in out echoString_Proxy ;
                  mesg : in Corba.String) ;

   function Operation(Self : in echoString_Proxy )
                      return Corba.String ;

   function Align_Size(Self : in echoString_Proxy ;
                       Size_In : in Corba.Unsigned_Long)
                       return Corba.Unsigned_Long ;

   procedure Marshal_Arguments(Self : in echoString_Proxy ;
                               Giop_Client : in out Giop_C.Object) ;

   procedure Unmarshal_Returned_Values(Self : in out echoString_Proxy ;
                                       Giop_Client : in out Giop_C.Object) ;

   function Get_Result (Self : in echoString_Proxy)
                        return Corba.String; 


private 
   type echoString_Proxy is new OmniProxyCallDesc.Object with record 
      mesg : Corba.String_Ptr := null ;
      Private_Result : Corba.String_Ptr := null;
   end record; 
   procedure Finalize(Self : in out echoString_Proxy) ;

end Echo.Proxies ;
