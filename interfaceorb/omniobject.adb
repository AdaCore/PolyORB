-----------------------------------------------------------------------
----                                                               ----
----                      AdaBroker                                ----
----                                                               ----
----     This package is wrapped around a C++ class whose name     ----
----   is Ada_OmniObject.                                          ----
----     It provides two types of methods : the C functions        ----
----   of the Ada_OmniObject class and their equivalent in         ----
----   Ada. (he first ones have a C_ prefix.)                      ----
----     In addition, there is a raise_ada_exception function      ----
----   that allows C functions to raise the ada No_Initialisation  ----
----   exception.                                                  ----
----                                                               ----
----                                                               ----
----                  package body omniObject                      ----
----                                                               ----
----   authors : Sebastien Ponce, Fabien Azavant                   ----
----   date    : 02/17/99                                          ----
----                                                               ----
----                                                               ----
-----------------------------------------------------------------------

with Ada.Unchecked_Conversion ;
with Rope ;
with Corba.Object, Giop_s ;
with Interfaces.C ; use Interfaces.C ;
--with OmniObjectManager.Address_To_Access ;


package body OmniObject is

   -- Is_Proxy
   -----------
   function Is_Proxy (Self : in Object'Class)
                      return Boolean is
   begin
      -- calls the C function and makes an Ada Boolean of the Result
      return (C_Is_Proxy(Self) /= Sys_Dep.C_False) ;
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


   -- Address_To_OmniObjectManager
   -------------------------------
   package Address_To_OmniObjectManager is
     new System.Address_To_Access_Conversions (OmniObjectManager.Object) ;
   -- needed to interface System.Address and Address_To_OmniObjectManager.Object

   -- Init
   -------
   procedure Init (Self : in out Object'Class ;
                   Manager : in OmniObjectManager.Object) is
      C_Manager : System.Address ;
      Manager_Aliased : aliased OmniObjectManager.Object ;
   begin
      -- make Manager aliased, ...
      Manager_Aliased := Manager ;
      -- ... transforms the arguments in a C type ...
--      C_Manager := Address_To_OmniObjectManager.To_Address(Manager_Aliased'access) ;
      C_Manager := Manager'Address ;
      -- ... and calls the C procedure
      C_Init (Self, C_Manager) ;
   end;


   -- Address_To_Rope
   ------------------
   package Address_To_Rope is
     new System.Address_To_Access_Conversions (Rope.Object) ;
   -- needed to interface System.Address and Address_To_Rope.Object


   -- Address_To_Octet
   -------------------
   package Address_To_Octet is
     new System.Address_To_Access_Conversions (Corba.Octet) ;
   -- needed to interface System.Address and Address_To_Octet.Object


   -- Address_To_Tagged_Profile_List
   ---------------------------------
   package Address_To_Tagged_Profile_List is
     new System.Address_To_Access_Conversions (Iop.Tagged_Profile_List) ;
   -- needed to interface System.Address and Address_To_Octet.Object


   -- C_To_Ada_Unsigned_Long
   -------------------------
   function C_To_Ada_Unsigned_Long is
     new Ada.Unchecked_Conversion (Corba.Unsigned_Long,
                                    Interfaces.C.Unsigned_Long) ;
   -- needed to change C type Interfaces.C.Unsigned_Long into
   -- ada type Corba.Unsigned_Long


   -- Init
   -------
   procedure Init (Self : in out Object'Class ;
                   RepoId : in String ;
                   R : in Rope.Object ;
                   Key : in Corba.Octet ;
                   Keysize : in Corba.Unsigned_Long ;
                   Profiles : in Iop.Tagged_Profile_List ;
                   Release : Corba.Boolean ) is
      R_Aliased : aliased Rope.Object ;
      Key_Aliased : aliased Corba.Octet ;
      Profiles_Aliased : aliased Iop.Tagged_Profile_List ;
      C_RepoId : Interfaces.C.Strings.Chars_Ptr ;
      C_R : System.Address ;
      C_Key : System.Address ;
      C_Keysize : Interfaces.C.Unsigned_Long ;
      C_Profiles : System.Address ;
      C_Release : Sys_Dep.C_Boolean ;
   begin
      -- make R, Key, Profiles aliased, ...
      R_Aliased := R ;
      Key_Aliased := Key ;
      Profiles_Aliased := Profiles ;
      -- transforms the arguments in a C type ...
      C_RepoId := Interfaces.C.Strings.New_String (RepoId) ;
      C_R := Address_To_Rope.To_Address (R_Aliased'Access) ;
      C_Key := Address_To_Octet.To_Address (Key_Aliased'Access) ;
      C_Keysize := C_To_Ada_Unsigned_Long (Keysize) ;
      C_Profiles := Address_To_Tagged_Profile_List.To_Address (Profiles_Aliased'Access) ;
      C_Release := Sys_Dep.Boolean_Ada_To_C (Release) ;
      -- ... and calls the C procedure
      C_Init (Self,
              C_RepoId,
              C_R,
              C_Key,
              C_Keysize,
              C_Profiles,
              C_Release) ;
   end;


   -- Address_To_omniRopeAndKey
   ----------------------------
   package Address_To_OmniRopeAndKey is
     new System.Address_To_Access_Conversions (OmniRopeAndKey.Object) ;
   -- needed to interface System.Address and Omniropeandkey.Object


   -- Set_Rope_And_Key
   -------------------
   procedure Set_Rope_And_Key (Self : in out Object'Class ;
                               L : in Omniropeandkey.Object ;
                               KeepIOP : in Corba.Boolean := True) is
      L_Ptr : Omniropeandkey.Object_Ptr := new Omniropeandkey.Object'(L) ;
      C_L : System.Address;
      C_KeepIOP : Interfaces.C.Unsigned_Char ;
   begin
      -- transforms the arguments in a C type ...
      C_L := Address_To_OmniRopeAndKey.To_Address(Address_To_OmniRopeAndKey.Object_Pointer (L_Ptr)) ;
--C_L := L'Address ;
      C_KeepIOP := Sys_Dep.Boolean_Ada_To_C (KeepIOP) ;
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
      return (C_Get_Rope_And_Key(Self,C_L) /= Sys_Dep.C_False) ;
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
      Ada_Orls_Ptr : Address_To_Giop_S.Object_Pointer ;
      Ada_Orl_Op : String := Interfaces.C.Strings.Value(Orl_OP) ;
      Ada_Orl_Response_Expected : Corba.Boolean ;
      Ada_Result : Boolean ;
   begin
      -- transforms the arguments in a Ada type ...
      Ada_Orls := Address_To_Giop_S.To_Pointer(Orls).all ;
      Ada_Orl_Response_Expected := Orl_Response_Expected /= 0;
      -- ... calls the ada function ...
      Ada_Result := Dispatch (Self.Corba_Object_Ptr,
                              Ada_Orls,
                              Ada_Orl_Op,
                              Ada_Orl_Response_Expected) ;
      -- ... and return a C value
      return Sys_Dep.Boolean_Ada_To_C (Ada_Result) ;
   end ;


end OmniObject ;


