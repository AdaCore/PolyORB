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


package omniProxyCallWrapper is

   type Object is limited private;

   procedure Invoke (O : in OmniObject.Ref'Class,
                     Call_Desc : in out OmniProxyCallDesc);
   -- wrapper around void invoke(omniObject* o, OmniProxyCallDesc& call_desc)
   -- in proxyCall.cc L 46


private


end omniproxyCallWrapper ;
