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

with Ada.Exceptions ;
with Ada.Unchecked_Conversion ;
with System.Address_To_Access_Conversions ;


package body OmniObject is

   --------------------------------------------------
   ---              Object is the                 ---
   ---        equivalent of the C++ class         ---
   --------------------------------------------------

   -- Init
   -------
   procedure Init (Self : in out Object'Class ;
                   Manager : in OmniObjectManager.Object) is
      C_Manager : System.Address ;
   begin
      -- transforms the arguments into a C type ...
      C_Manager := Manager'Address ;
      -- ... and calls the C procedure
      C_Init (Self, C_Manager) ;
   end;


   -- Ada_To_C_Unsigned_Long
   -------------------------
   function Ada_To_C_Unsigned_Long is
     new Ada.Unchecked_Conversion (Corba.Unsigned_Long,
                                   Interfaces.C.Unsigned_Long) ;
   -- needed to change ada type Corba.Unsigned_Long
   -- into C type Interfaces.C.Unsigned_Long


   -- Init
   -------
   procedure Init (Self : in out Object'Class ;
                   RepoId : in String ;
                   R : in Rope.Object ;
                   Key : in Corba.Octet ;
                   Keysize : in Corba.Unsigned_Long ;
                   Profiles : in Iop.Tagged_Profile_List ;
                   Release : Corba.Boolean ) is
      C_RepoId : Interfaces.C.Strings.Chars_Ptr ;
      C_R : System.Address ;
      C_Key : System.Address ;
      C_Keysize : Interfaces.C.Unsigned_Long ;
      C_Profiles : System.Address ;
      C_Release : Sys_Dep.C_Boolean ;
   begin
      -- transforms the arguments into a C type ...
      C_RepoId := Interfaces.C.Strings.New_String (RepoId) ;
      C_R := R'Address ;
      C_Key := Key'Address ;
      C_Keysize := Ada_To_C_Unsigned_Long(Keysize) ;
      C_Profiles := System.Address(Profiles) ;
      C_Release := Sys_Dep.Boolean_Ada_To_C (Release) ;
      -- ... and calls the C procedure
      C_Init2 (Self,
               C_RepoID,
               C_R,
               C_Key,
               C_Keysize,
               C_Profiles,
               C_Release) ;
   end;


   -- PR_IRRepositoryId
   --------------------
   procedure PR_IRRepositoryId(Self : in Object'Class ;
                               RepositoryId : in String ) is
      C_RepositoryId : Interfaces.C.Strings.Chars_Ptr;
   begin
      -- transforms the arguments into a C type ...
      C_RepositoryId := Interfaces.C.Strings.New_String(RepositoryId) ;
      -- ... and calls the C procedure
      C_PR_IRRepositoryId (Self,C_RepositoryId) ;
   end ;


   -- Set_Rope_And_Key
   -------------------
   procedure Set_Rope_And_Key (Self : in out Object'Class ;
                               L : in Omniropeandkey.Object ;
                               KeepIOP : in Boolean) is
      C_L : System.Address;
      C_KeepIOP : Sys_Dep.C_Boolean ;
   begin
      -- transforms the arguments into a C type ...
      C_L := L'Address ;
      C_KeepIOP := Sys_Dep.Boolean_Ada_To_C (KeepIOP) ;
      -- ... and calls the C procedure
      C_Set_Rope_And_Key (Self,C_L,C_KeepIOP) ;
   end ;


   -- Get_Rope_And_Key
   -------------------
   function Get_Rope_And_Key (Self : in Object'Class ;
                              L : in Omniropeandkey.Object)
                              return Boolean is
      C_L : System.Address;
      C_Result : Sys_Dep.C_Boolean ;
   begin
      -- transforms the arguments in a C type ...
      C_L := L'Address ;
      -- ... calls the C function ...
      C_Result := C_Get_Rope_And_Key(Self,C_L) ;
      -- ... and transforms the result into an Ada type
      return Sys_Dep.Boolean_C_To_Ada (C_Result) ;
   end ;


   -- Is_Proxy
   -----------
   function Is_Proxy (Self : in Object'Class)
                      return Boolean is
      C_Result : Sys_Dep.C_Boolean ;
   begin
      -- calls the C function ...
      C_Result := C_Is_Proxy(Self) ;
      -- ... and transforms the result into an Ada type
      return Sys_Dep.Boolean_C_To_Ada (C_Result) ;
   end ;


   -- Address_To_Giop_S
   --------------------
   package Address_To_Giop_S is
     new System.Address_To_Access_Conversions (Giop_S.Object) ;
   -- needed to convert System.Address into Giop_S.Object


   -- C_Dispatch
   -------------
   function C_Dispatch (Self : in Object'Class ;
                        Orls : in System.Address ;
                        Orl_Op : in Interfaces.C.Strings.Chars_Ptr ;
                        Orl_Response_Expected : in Sys_Dep.C_Boolean)
                        return Sys_Dep.C_Boolean is
      Ada_Orls_Ptr : Address_To_Giop_S.Object_Pointer ;
      Ada_Orl_Op : String := Interfaces.C.Strings.Value(Orl_OP) ;
      Ada_Orl_Response_Expected : Boolean ;
      Ada_Result : Boolean ;
   begin
      -- transforms the arguments in a Ada type, ...
      Ada_Orls_Ptr := Address_To_Giop_S.To_Pointer(Orls) ;
      Ada_Orl_Response_Expected := Sys_Dep.Boolean_C_To_Ada (Orl_Response_Expected) ;
      -- ... calls the ada function ...
      Ada_Result := Dispatch (Self,
                              Ada_Orls_Ptr.All,
                              Ada_Orl_Op,
                              Ada_Orl_Response_Expected) ;
      -- ... and transforms the result into a C type
      return Sys_Dep.Boolean_Ada_To_C (Ada_Result) ;
   end ;



end OmniObject ;


