-----------------------------------------------------------------------
----                                                               ----
----                  AdaBroker                                    ----
----                                                               ----
----                  package omniProxyCallWrapper                 ----
----                                                               ----
----   authors : Sebastien Ponce, Fabien Azavant                   ----
----   date    :                                                   ----
----                                                               ----
----                                                               ----
-----------------------------------------------------------------------


with Corba.Object ;
with Omniproxycalldesc ;

package omniProxyCallWrapper is

   procedure Invoke (The_Obj : in Corba.Object.Ref'Class ;
                     Call_Desc : in out OmniProxyCallDesc.Object'Class ) ;
   -- reimplemented in Ada to call the C++ ORB
   -- (modified by Fabien)
   --
   -- previous solution :
   -- wrapper around void invoke(omniObject* o, OmniProxyCallDesc& call_desc)
   -- in proxyCall.cc L 46


   procedure One_Way(The_Obj : in Corba.Object.Ref'Class ;
                     Call_Desc : in out OmniProxyCallDesc.Object'Class) ;
   -- reimplemented in Ada to call the C++ ORB
   -- see proxyCall.cc L181



end omniproxyCallWrapper ;







