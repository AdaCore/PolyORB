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

with Omniproxycalldesc, omniobject ;

package omniProxyCallWrapper is

   type Object is limited private;

   procedure Invoke (The_Obj : in OmniObject.Object ;
                     Call_Desc : in out OmniProxyCallDesc.Object'Class ) ;
   -- reimplemented in Ada to call the C++ ORB
   -- (modified by Fabien)
   --
   -- previous solution :
   -- wrapper around void invoke(omniObject* o, OmniProxyCallDesc& call_desc)
   -- in proxyCall.cc L 46


   procedure One_Way(The_Obj : in OmniObject.Object'Class ;
                     Call_Desc : in out OmniProxyCallDesc.Object) ;
   -- reimplemented in Ada to call the C++ ORB
   -- see proxyCall.cc L181



private


end omniproxyCallWrapper ;
