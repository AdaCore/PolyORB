----------------------------------------------------------------------------
----                                                                    ----
----     This in an example which is hand-written                       ----
----     for the echo object                                            ----
----                                                                    ----
----                package echo_proxies                                ----
----                                                                    ----
----                authors : Fabien Azavant, Sebastien Ponce           ----
----                                                                    ----
----------------------------------------------------------------------------

with Corba ;
with Giop_C ;
with Omniproxycalldesc ;

package Echo.Proxies is


   type EchoString_Proxy is new OmniProxyCallDesc.Object(10) with private ;

   function Create(Operation : Standard.String ;
                   Arg : Corba.String ) return EchoString_Proxy ;

   function Aligned_Size(Self: in EchoString_Proxy ;
                         Size_In: in Corba.Unsigned_Long)
                         return Corba.Unsigned_Long ;

   procedure Marshal_Arguments(Self: in EchoString_Proxy ;
                               Giop_Client: in out Giop_C.Object) ;

   procedure Unmarshal_Returned_Values(Self: in out EchoString_Proxy ;
                                       Giop_Client: in Giop_C.Object) ;

   function Get_Result (Self : in EchoString_Proxy)
                    return CORBA.String;


private

   -- Beware : fields of this record are dinamically allocated
   -- they must be released
   type EchoString_Proxy is new OmniProxyCallDesc.Object(10) with record
      Arg_Msg : Corba.String_Ptr := null ;
      Private_Result : Corba.String_Ptr := null ;
   end record ;

end Echo.Proxies ;
