-----------------------------------------------------------------------
----                                                               ----
----                  AdaBroker                                    ----
----                                                               ----
----                  package omniObjectManager                    ----
----                                                               ----
----   authors : Sebastien Ponce, Fabien Azavant                   ----
----   date    : 02/08/99                                          ----
----                                                               ----
----                                                               ----
-----------------------------------------------------------------------

with System ;

package OmniObjectManager is

--   package OmniObjectManager.Address_To_Access is
--     new System.Address_To_Access_Conversions (OmniObjectManager.Object) ;
   -- needed to interface System.Address and Omniropeandkey.Object

   type Object is private ;

   type Object_Ptr is access Object ;

   function nilObjectManager return OmniObjectManager.Object ;
   -- wrapper around    static omniObjectManager*  nilObjectManager();
   -- in omniInternal.h L 514

private

   type Object is null record ;

   end OmniObjectManager ;

