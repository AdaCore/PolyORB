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

with System, Interfaces.C, Interfaces.C.Strings ;
with Corba, OmniObjectManager, Omniropeandkey ;

package body OmniObject is

   function Is_Proxy return Boolean is
      function C_Is_Proxy return Interfaces.C.Unsigned_Char ;
      -- here is supposed that HAS_Cplusplus_Bool is'nt defined
      -- cf definition of type _CORBA_Boolean in CORBA_basetypes.h L59
      pragma Import (C,C_Is_Proxy,"is_proxy__C10omniObject") ;
   begin
      return (Is_Proxy() \= 0) ;
   end ;

   procedure PR_IRRepositoryId(RepositoryId : in String ) is
      procedure C_PR_IRRepositoryId (C_RepositoryId : in Chars_Ptr) ;
      pragma Import (C,C_PR_IRRepositoryId,"is_proxy__C10omniObject") ;
   begin
      C_PR_IRRepositoryId ( New_String(RepositoryId) ) ;
      return;
   end ;

   procedure Set_Rope_And_Key (Self : in out Object ;
                            L : in out Omniropeandkey.Object ;
                            KeepIOP : Corba.boolean
                           ) is
      procedure C_Set_Rope_And_Key () ;
      pragma Import (C,C_Set_Rope_And_Key,
                     "setRopeAndKey__10omniObjectRC14omniRopeAndKeyb") ;
   begin
      C_Set_Rope_And_Key () ;
   end ;

   procedure Get_Rope_And_Key (Self : in Object ;
                           L : in out Omniropeandkey.Object ;
                           Result : out Corba.Boolean) ;
   -- wrapper around _CORBA_Boolean getRopeAndKey(omniRopeAndKey& l) const;
   -- in omniInternal.h L 338

   procedure Assert_Object_Existent (Self : in Object) ;
   -- wrapper around   void assertObjectExistent();
   -- in omniInternal.h L 356

   procedure Reset_Rope_And_Key (Self : in Object);
   -- wrapper around void resetRopeAndKey();
   -- in omniInternal.h L 332

   procedure Init (Self : in out Object ;
                   Manager : in OmniObjectManager.Object);
   -- wrapper around   omniObject(omniObjectManager*p =0);
   -- in omniInternal.h L 294


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
