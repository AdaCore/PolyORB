-----------------------------------------------------------------------
----                                                               ----
----                  AdaBroker                                    ----
----                                                               ----
----                  package omniObject                           ----
----                                                               ----
----   authors : Sebastien Ponce, Fabien Azavant                   ----
----   date    : 02/08/99                                          ----
----                                                               ----
----                                                               ----
-----------------------------------------------------------------------

with System, Interfaces.C.Strings ;
with Corba, OmniObjectManager, Omniropeandkey ;

package OmniObject is

   type Object is private ;

   function Is_Proxy (This : in Object)
                     return Boolean ;
   -- wrapper around   inline _CORBA_Boolean is_proxy()
   -- in omniInternal.h L384

   procedure PR_IRRepositoryId(This : in Object;
                               RepositoryId : in String ) ;
   -- wrapper around   void  PR_IRRepositoryId(const char* s);
   -- in omniInternal.h L 306

   procedure Init (This : in out Object ;
                   Manager : in OmniObjectManager.Object);
   -- wrapper around   omniObject(omniObjectManager*p =0);
   -- in omniInternal.h L 294

   procedure Set_Rope_And_Key (This : in out Object ;
                               L : in out Omniropeandkey.Object ;
                               KeepIOP : Corba.boolean
                              ) ;
   -- wrapper around void setRopeAndKey(const omniRopeAndKey& l,
   --                                   _CORBA_Boolean keepIOP=1);
   -- in omniInternal.h L 328


   function Get_Rope_And_Key (Self : in Object ;
                               L : in out Omniropeandkey.Object)
                              return COrba.Boolean ;
   -- wrapper around _CORBA_Boolean getRopeAndKey(omniRopeAndKey& l) const;
   -- in omniInternal.h L 338

   procedure Assert_Object_Existent (Self : in Object) ;
   -- wrapper around   void assertObjectExistent();
   -- in omniInternal.h L 356

   procedure Reset_Rope_And_Key (Self : in Object);
   -- wrapper around void resetRopeAndKey();
   -- in omniInternal.h L 332

private

   type Object is tagged record
      Table : Vtable_Ptr;
   end record;
   pragma CPP_Class (Object);
   pragma CPP_Vtable (Object,Table,2);


   package Address_To_Giop_S is new System.Address_To_Access_Conversions (Giop_S.Object) ;

   function Dispatch (This : in Object ;
                      Orls : in Giop_S.Object ;
                      Orl_Op : in Corba.String ;
                      Orl_Response_Expected : in Corba.Boolean)
                      return Corba.Boolean;
   -- in place of CORBA::Boolean
   --             dispatch_ada (GIOP_S &_ORL_s, const char *_ORL_op,
   --                           CORBA::Boolean _ORL_response_expected)


end OmniObject ;


