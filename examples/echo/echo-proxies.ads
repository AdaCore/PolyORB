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

with Giop_C ;
with Omniproxycalldesc ;
with Proxyobjectfactory ;
with Rope ;
with Iop ;

package Echo.Proxies is


   --------------------------------------------------
   ----           object factory                 ----
   --------------------------------------------------
   type ObjectFactory is new Proxyobjectfactory.Object with null record ;

   function New_Proxy_Object(Self : in ObjectFactory ;
                             R : in Rope.object ;
                             Key : in Corba.Octet ;
                             Key_Size : in Corba.Unsigned_Long ;
                             Profiles : in Iop.Tagged_Profile_List ;
                             Release : in Corba.Boolean)
                             return Echo.Ref ;

   function Nil(Self : in ObjectFactory) return Echo.Ref ;

   function Is_A(Self : in ObjectFactory ;
                 Base_RepoID : in String) return Corba.Boolean  ;

   Static_Factory : constant ObjectFactory := (Proxyobjectfactory.Object with null record) ;

   --------------------------------------------------
   ----        function EchoString               ----
   --------------------------------------------------

   type EchoString_Proxy is new OmniProxyCallDesc.Object with private ;

   function Create(Arg : Corba.String ) return EchoString_Proxy ;

   procedure Free(Self : in out EchoString_Proxy) ;

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

   type EchoString_Proxy is new OmniProxyCallDesc.Object with record
      Arg_Msg : Corba.String_Ptr := null ;
      Private_Result : Corba.String_Ptr := null ;
   end record ;

end Echo.Proxies ;

