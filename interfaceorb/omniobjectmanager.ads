-----------------------------------------------------------------------
----                                                               ----
----                  AdaBroker                                    ----
----                                                               ----
----                  package omniObjectManager                    ----
----                                                               ----
----   authors : Sebastien Ponce, Fabien Azavant                   ----
----   date    :                                                   ----
----                                                               ----
----                                                               ----
-----------------------------------------------------------------------


package OmniObjectManager is

   type Object is limited private ;

   function nilObjectManager () return OmniObjectManager.Object ;
   -- wrapper around    static omniObjectManager*  nilObjectManager();
   -- in omniInternal.h L 514

private



end OmniObjectManager ;
