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

with System, Interfaces.C ;
use Interfaces.C;

package body OmniObject is

   function Is_Proxy (Self : in Object'Class)
                      return Boolean is
      function C_Is_Proxy (Self : in Object'Class)
                           return Interfaces.C.Unsigned_Char ;
      -- here is supposed that HAS_Cplusplus_Bool is'nt defined
      -- cf definition of type _CORBA_Boolean in CORBA_basetypes.h L59
      pragma Import (C,C_Is_Proxy,"is_proxy__C10omniObject") ;
   begin
      return (C_Is_Proxy(Self) /= 0) ;
   end ;

   procedure PR_IRRepositoryId(Self : in Object'Class ;
                               RepositoryId : in String ) is
      procedure C_PR_IRRepositoryId (Self : in Object'Class ;
                                     C_RepositoryId : in Interfaces.C.Strings.Chars_Ptr) ;
      pragma Import (C,C_PR_IRRepositoryId,"is_proxy__C10omniObject") ;
   begin
      C_PR_IRRepositoryId (Self,Interfaces.C.Strings.New_String(RepositoryId) ) ;
      return;
   end ;

   procedure Init (Self : in out Object'Class ;
                   Manager : in OmniObjectManager.Object) is
      I : Integer;
   begin
      I := 0;
   -- wrapper around   omniObject(omniObjectManager*p =0);
   -- in omniInternal.h L 294
   end;

   package Address_To_omniRopeAndKey is
     new System.Address_To_Access_Conversions (Omniropeandkey.Object) ;


   procedure Set_Rope_And_Key (Self : in out Object'Class ;
                               L : in out Omniropeandkey.Object ;
                               KeepIOP : in Corba.Boolean := True) is
      procedure C_Set_Rope_And_Key (Self : in out Object'Class ;
                                    L : in out System.Address;
                                    KeepIOP : in Interfaces.C.Unsigned_Char) ;
      pragma Import (C,C_Set_Rope_And_Key,
                     "setRopeAndKey__10omniObjectRC14omniRopeAndKeyb") ;
      Boubool : Interfaces.C.Unsigned_Char ;
      C_L : System.Address;
   begin
      if KeepIOP
      then
         Boubool := 1 ;
      else
         Boubool := 0;
      end if;
      C_L := Address_To_OmniRopeAndKey.To_Address(L'Access);
      C_Set_Rope_And_Key (Self,C_L,Boubool) ;
   end ;


   function Get_Rope_And_Key (Self : in Object'Class ;
                              L : in Omniropeandkey.Object)
                              return COrba.Boolean is
      function C_Get_Rope_And_Key (Self : in Object'Class ;
                                   L : in System.Address)
                                   return Interfaces.C.Unsigned_Char ;
      pragma Import (CPP,C_Get_Rope_And_Key,
                     "getRopeAndKey__C10omniObjectR14omniRopeAndKey") ;
      C_L : System.Address;
   begin
      C_L := Address_To_OmniRopeAndKey.To_Address(L'Access);
      return (C_Get_Rope_And_Key(Self,C_L) /= 0) ;
   end ;


   function Dispatch (Self : in Object'Class ;
                      Orls : in System.Address ;
                      Orl_Op : in Interfaces.C.Strings.Chars_Ptr ;
                      Orl_Response_Expected : in Interfaces.C.Unsigned_Char)
                      return Interfaces.C.Unsigned_Char is
      Boubool : Corba.Boolean ;
   begin
      Boubool := Orl_Response_Expected /= 0;
      Boubool := Dispatch (Self.AdaObject,
                           Address_To_Giop_S.To_Pointer(Orls),
                           Interfaces.C.Strings.Value(Orl_OP),
                           Boubool) ;
      if Boubool then return 1 else return 0 ;
      end ;
   pragma Export (CPP,Dispatch,
                  "dispatch__10omniObjectR6GIOP_SPCcb");

end OmniObject ;

