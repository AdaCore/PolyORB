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

   function Is_Proxy (This : in Object)
                      return Boolean is
      function C_Is_Proxy (This : in Object)
                           return Interfaces.C.Unsigned_Char ;
      -- here is supposed that HAS_Cplusplus_Bool is'nt defined
      -- cf definition of type _CORBA_Boolean in CORBA_basetypes.h L59
      pragma Import (C,C_Is_Proxy,"is_proxy__C10omniObject") ;
   begin
      return (C_Is_Proxy(This) \= 0) ;
   end ;

   procedure PR_IRRepositoryId(This : in Object;
                               RepositoryId : in String ) is
      procedure C_PR_IRRepositoryId (This : in Object;
                                     C_RepositoryId : in Chars_Ptr) ;
      pragma Import (C,C_PR_IRRepositoryId,"is_proxy__C10omniObject") ;
   begin
      C_PR_IRRepositoryId (This,New_String(RepositoryId) ) ;
      return;
   end ;

   procedure Init (Self : in out Object ;
                   Manager : in OmniObjectManager.Object);
   -- wrapper around   omniObject(omniObjectManager*p =0);
   -- in omniInternal.h L 294

   package Address_To_omniRopeAndKey is new System.Address_To_Access_Conversions (Omniropeandkey.Object) ;

   procedure Set_Rope_And_Key (This : in out Object ;
                               L : in out Omniropeandkey.Object ;
                               KeepIOP : in Corba.Boolean := Corba.True) is
      procedure C_Set_Rope_And_Key (This : in out Object ;
                                    L : in out Address_To_OmniRopeAndKey.Object_Pointer;
                                    KeepIOP : in Interfaces.C.Unsigned_Char) ;
      pragma Import (C,C_Set_Rope_And_Key,
                     "setRopeAndKey__10omniObjectRC14omniRopeAndKeyb") ;
      Boubool : Interfaces.C.Unsigned_Char ;
   begin
      if Orl_Response_Expected
      then
         Boubool := 1 ;
      else
         Boubool := 0;
      end if;
      C_Set_Rope_And_Key (This,To_Pointer(L'access),Boubool) ;
   end ;

   function Get_Rope_And_Key (Self : in Object ;
                              L : in out Omniropeandkey.Object)
                              return COrba.Boolean is
      function C_Get_Rope_And_Key (This : in Object ;
                                   L : in out Address_To_OmniRopeAndKey.Object_Pointer)
                                   return Interfaces.C.Unsigned_Char ;
      pragma Import (C,C_Get_Rope_And_Key,
                     "getRopeAndKey__C10omniObjectR14omniRopeAndKey") ;
   begin
      return (C_Get_Rope_And_Key(This,To_Pointer(L'access)) \= 0) ;
   end ;

   procedure Assert_Object_Existent (Self : in Object) ;
   pragma Import (CPP,Assert_Object_Existent,
                  "assertObjectExistent__10omniObject");

   procedure Reset_Rope_And_Key (Self : in Object);
   pragma Import (CPP,Assert_Object_Existent,
                  "resetRopeAndKey__10omniObject");

private

   function Dispatch (This : in Object ;
                      Orls : in Giop_S.Object ;
                      Orl_Op : in Corba.String ;
                      Orl_Response_Expected : in Corba.Boolean)
                      return Corba.Boolean is
      function C_Dispatch (This : in Object ;
                           Orls : in Address_To_Giop_S.Object_Pointer ;
                           Orl_Op : in Interfaces.C.Strings.Chars_Ptr ;
                           Orl_Response_Expected : in Interfaces.C.Unsigned_Char)
                           return Interfaces.C.Unsigned_Char ;
      pragma Export (CPP,C_Dispatch,
                     "dispatch__10omniObjectR6GIOP_SPCcb");
      Boubool : Interfaces.C.Unsigned_Char ;
   begin
      if Orl_Response_Expected
      then
         Boubool := 1 ;
      else
         Boubool := 0;
      end if;
      return ((C_Dispatch(This,
                          Address_To_Giop_S.To_Pointer(Orls),
                          Interfaces.C.Strings.New_String(Orl_Op),
                          Boubool)) \= 0);
   end;

end OmniObject ;

