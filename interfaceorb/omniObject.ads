-----------------------------------------------------------------------
----                                                               ----
----                  AdaBroker                                    ----
----                                                               ----
----                  package omniObject                           ----
----                                                               ----
----   authors : Sebastien Ponce, Fabien Azavant                   ----
----   date    :                                                   ----
----                                                               ----
----                                                               ----
-----------------------------------------------------------------------


package OmniObject is

   type Object is limited private ;

   function Is_Proxy() return Boolean ;
   -- wrapper around   inline _CORBA_Boolean is_proxy()
   -- in omniInternal.h L384

   procedure PR_IRRepositoryId(in String RepositoryId) ;
   -- wrapper around   void  PR_IRRepositoryId(const char* s);
   -- in omniInternal.h L 306

   procedure Init (Self : in out Object,Manager : in OmniObjectManager.Object);
   -- wrapper around   omniObject(omniObjectManager*p =0);
   -- in omniInternal.h L 294

   procedure SetRopeAndKey (Self : in Object, ...) ;
   -- wrapper around void setRopeAndKey(const omniRopeAndKey& l,_CORBA_Boolean keepIOP=1);
   -- in omniInternal.h L 328

   function GetRopeAndKey (Self : in Object, ...) return ... ;
   -- wrapper around _CORBA_Boolean getRopeAndKey(omniRopeAndKey& l) const;
   -- in omniInternal.h L 338


private



end OmniObject ;
