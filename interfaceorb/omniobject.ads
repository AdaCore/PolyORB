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

with Interfaces.CPP, Interfaces.C.Strings ;
with Corba, OmniObjectManager, Omniropeandkey, Giop_s ;
with Corba.Object ;
with System.Address_To_Access_Conversions ;

package OmniObject is

   type Object is tagged record
      AdaObject : Corba.Object.Ref_Access;
      Table : Interfaces.CPP.Vtable_Ptr;
   end record;

   pragma CPP_Class (Object);
   pragma CPP_Vtable (Object,Table,2);

   function Is_Proxy (Self : in Object'Class)
                     return Boolean ;
   -- wrapper around   inline _CORBA_Boolean is_proxy()
   -- in omniInternal.h L384

   procedure PR_IRRepositoryId(Self : in Object'Class;
                               RepositoryId : in String ) ;
   -- wrapper around   void  PR_IRRepositoryId(const char* s);
   -- in omniInternal.h L 306

   procedure Init (Self : in out Object'Class ;
                   Manager : in OmniObjectManager.Object);
   -- wrapper around   omniObject(omniObjectManager*p =0);
   -- in omniInternal.h L 294

   procedure Set_Rope_And_Key (Self : in out Object'Class ;
                               L : in out Omniropeandkey.Object ;
                               KeepIOP : in Corba.Boolean := True) ;
   -- wrapper around void setRopeAndKey(const omniRopeAndKey& l,
   --                                   _CORBA_Boolean keepIOP=1);
   -- in omniInternal.h L 328


   function Get_Rope_And_Key (Self : in Object'Class ;
                               L : in Omniropeandkey.Object)
                              return COrba.Boolean ;
   -- wrapper around _CORBA_Boolean getRopeAndKey(omniRopeAndKey& l) const;
   -- in omniInternal.h L 338

   procedure Assert_Object_Existent (Self : in Object'Class) ;
   pragma Import (CPP,Assert_Object_Existent,
                  "assertObjectExistent__10omniObject");
   -- wrapper around   void assertObjectExistent();
   -- in omniInternal.h L 356

   procedure Reset_Rope_And_Key (Self : in Object'Class);
   pragma Import (CPP,Reset_Rope_And_Key,
                  "resetRopeAndKey__10omniObject");
   -- wrapper around void resetRopeAndKey();
   -- in omniInternal.h L 332

private

   package Address_To_Giop_S is
     new System.Address_To_Access_Conversions (Giop_S.Object) ;

   function Dispatch (Self : in Object'Class ;
                      Orls : in Giop_S.Object ;
                      Orl_Op : in String ;
                      Orl_Response_Expected : in Corba.Boolean)
                      return Corba.Boolean ;
   -- in place of CORBA::Boolean
   --             dispatch_ada (GIOP_S &_ORL_s, const char *_ORL_op,
   --                           CORBA::Boolean _ORL_response_expected)


end OmniObject ;



