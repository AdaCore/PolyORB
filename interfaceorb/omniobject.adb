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

with Corba.Object, Giop_s ;
with System, Interfaces.C ;
use Interfaces.C ;

package body OmniObject is


   -- Is_Proxy
   -----------
   function Is_Proxy (Self : in Object'Class)
                      return Boolean is
   begin
      -- calls the C function and makes an Ada Boolean of the Result
      return (C_Is_Proxy(Self) /= 0) ;
   end ;


   -- PR_IRRepositoryId
   --------------------
   procedure PR_IRRepositoryId(Self : in Object'Class ;
                               RepositoryId : in String ) is
      C_RepositoryId : Interfaces.C.Strings.Chars_Ptr;
   begin
      -- transforms the arguments in a C type ...
      C_RepositoryId := Interfaces.C.Strings.New_String(RepositoryId) ;
      -- ... and calls the C procedure
      C_PR_IRRepositoryId (Self,C_RepositoryId) ;
   end ;


   -- Init
   -------
   procedure Init (Self : in out Object'Class ;
                   Manager : in OmniObjectManager.Object) is
      I : Integer;
   begin
      C_Init ();
   end;

   -- Init
   -------
   procedure Init (Self : in out Object'Class ;
                   RepoId : in String ;
                   R : in Rope.Object ;
                   Key : in Corba.Octet ;
                   Keysize : in Corba.Long ;
                   Profiles : in Iop.TaggedProfileList ;
                   Release : Corba.Boolean ) is
   begin
      C_Init2 ();
   end;


   -- Address_To_omniRopeAndKey
   ----------------------------
   package Address_To_omniRopeAndKey is
     new System.Address_To_Access_Conversions (Omniropeandkey.Object) ;
   -- needed to convert System.Address into Omniropeandkey.Object


   -- Set_Rope_And_Key
   -------------------
   procedure Set_Rope_And_Key (Self : in out Object'Class ;
                               L : in out Omniropeandkey.Object ;
                               KeepIOP : in Corba.Boolean := True) is
      C_L : System.Address;
      C_KeepIOP : Interfaces.C.Unsigned_Char ;
   begin
      -- transforms the arguments in a C type ...
      C_L := Address_To_OmniRopeAndKey.To_Address(L'Access);
      if KeepIOP
      then
         Boubool := 1 ;
      else
         Boubool := 0;
      end if;
      -- ... and calls the C procedure
      C_Set_Rope_And_Key (Self,C_L,C_KeepIOP) ;
   end ;


   -- Get_Rope_And_Key
   -------------------
   function Get_Rope_And_Key (Self : in Object'Class ;
                              L : in Omniropeandkey.Object)
                              return Corba.Boolean is
      C_L : System.Address;
   begin
      -- transforms the arguments in a C type ...
      C_L := Address_To_OmniRopeAndKey.To_Address(L'Access);
      -- ... and calls the C function
      return (C_Get_Rope_And_Key(Self,C_L) /= 0) ;
   end ;


   -- Address_To_Corba_Object
   --------------------------
   package Address_To_Corba_Object is
     new System.Address_To_Access_Conversions (Corba.Object.Ref) ;
   -- needed to convert System.Address into Corba.Object.Ref


   -- Address_To_Giop_S
   --------------------
   package Address_To_Giop_S is
     new System.Address_To_Access_Conversions (Giop_S.Object) ;
   -- needed to convert System.Address into Giop_S.Object


   -- Dispatch
   -----------
   function C_Dispatch (Self : in Object'Class ;
                        Orls : in System.Address ;
                        Orl_Op : in Interfaces.C.Strings.Chars_Ptr ;
                        Orl_Response_Expected : in Interfaces.C.Unsigned_Char)
                        return Interfaces.C.Unsigned_Char is
      Ada_Orls : Giop_S.Object ;
      Ada_Orl_Op : String ;
      Ada_Orl_Response_Expected : Corba.Boolean ;
      Result : Boolean ;
   begin
      -- transforms the arguments in a Ada type ...
      Ada_Orls := Address_To_Giop_S.To_Pointer(Orls) ;
      Ada_Orl_Op := Interfaces.C.Strings.Value(Orl_OP) ;
      Ada_Orl_Response_Expected := Orl_Response_Expected /= 0;
      -- ... calls the ada function ...
      Result := Dispatch (Self.Corba_Object_Ptr,
                           Ada_Orls,
                           Ada_Orl_Op,
                           Ada_Orl_Response_Expected) ;
      -- ... and return a C value
      if Result
      then
         return 1 ;
      else
         return 0 ;
      end ;


end OmniObject ;

