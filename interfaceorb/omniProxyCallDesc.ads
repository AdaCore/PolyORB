-----------------------------------------------------------------------
----                                                               ----
----                  AdaBroker                                    ----
----                                                               ----
----                  package omniProxyCallDesc                    ----
----                                                               ----
----   authors : Sebastien Ponce, Fabien Azavant                   ----
----   date    :                                                   ----
----                                                               ----
----                                                               ----
-----------------------------------------------------------------------


package omniProxyCallDesc is

   type Object is limited private;

   procedure Init (Self : in out Object,
                     ...
                  );
   -- wrapper around   inline OmniProxyCallDesc(const char* op,
   --                                           size_t op_len,
   --                                           CORBA::Boolean has_exceptions = 0)
   -- In proxyCall.h L37

private


end omniproxyCallDesc ;
