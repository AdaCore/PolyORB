-----------------------------------------------------------------------
----                                                               ----
----                  AdaBroker                                    ----
----                                                               ----
----                  package body omniObject                      ----
----                                                               ----
----   authors : Sebastien Ponce, Fabien Azavant                   ----
----   date    : 02/08/99                                          ----
----                                                               ----
----                                                               ----
-----------------------------------------------------------------------

with System, Interfaces.C.Strings ;
with Corba, OmniObjectManager, Omniropeandkey ;

package body OmniObject is

   function Is_Proxy return Boolean;
   pragma Import (C,Is_Proxy,"is_proxy");

   procedure PR_IRRepositoryId(RepositoryId : in String ) ;
   -- wrapper around   void  PR_IRRepositoryId(const char* s);
   -- in omniInternal.h L 306

   procedure Init (Self : in out Object ;
                   Manager : in OmniObjectManager.Object);
   -- wrapper around   omniObject(omniObjectManager*p =0);
   -- in omniInternal.h L 294

   procedure SetRopeAndKey (Self : in out Object ;
                            L : in out Omniropeandkey.Object ;
                            KeepIOP : Corba.boolean
                           ) ;
   -- wrapper around void setRopeAndKey(const omniRopeAndKey& l,
   --                                   _CORBA_Boolean keepIOP=1);
   -- in omniInternal.h L 328


   procedure GetRopeAndKey (Self : in Object ;
                           L : in out Omniropeandkey.Object ;
                           Result : out Corba.Boolean) ;
   -- wrapper around _CORBA_Boolean getRopeAndKey(omniRopeAndKey& l) const;
   -- in omniInternal.h L 338

   procedure AssertObjectExistent (Self : in Object) ;
   -- wrapper around   void assertObjectExistent();
   -- in omniInternal.h L 356

   procedure ResetRopeAndKey (Self : in Object);
   -- wrapper around void resetRopeAndKey();
   -- in omniInternal.h L 332

private

   function Dispatch (Self : in System.address ;
                        Orls : in System.Address ;
                        Orl_Op : in Interfaces.C.Strings.Chars_Ptr ;
                        Orl_Response_Expected : in System.Address)
                      return System.Address;

   pragma Export (C,Dispatch,"dispatch_ada");
   -- in place of CORBA::Boolean
   --             dispatch_ada (GIOP_S &_ORL_s, const char *_ORL_op,
   --                           CORBA::Boolean _ORL_response_expected)


end OmniObject ;
