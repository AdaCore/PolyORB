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


   -----------------------------------------------
   -----------------------------------------------
   -----------------------------------------------
   --         Implemented_Object                --
   --       this is the type of local           --
   --      implementations of objects           --
   -- it is the root of all XXX.Impl.Object     --
   -----------------------------------------------
   -----------------------------------------------
   -----------------------------------------------

   -- Is_Nil
   ---------
   function Is_Nil(Self : in Implemented_Object) return Corba.Boolean is
   begin
      return Self.Omniobj = null ;
   end ;



   -- Initialize
   -------------
   procedure Initialize (Self: in out Implemented_Object) is
      type Ptr is access all Implemented_Object ;
      function To_Implemented_Object_Object_Ptr is
        new Ada.Unchecked_Conversion (Ptr, Implemented_Object_Ptr);
      Tmp : Object_Ptr := Object_Ptr_Constructor ;
   begin
      Tmp.all.Implobj := To_Implemented_Object_Object_Ptr(Self'Access) ;
      Self.Omniobj := Tmp ;
   end ;



   -- Adjust
   -----------
   procedure Adjust (Self: in out Implemented_Object) is
   begin
      if not Is_Nil(Self) then
         declare
            RepoId : Corba.String := Get_Repository_Id(Self.Omniobj.all) ;
         begin
            Initialize(Self) ;
            Set_Repository_Id(Self.Omniobj.all, RepoID) ;
         end ;
      end if ;
   end ;

   -- Finalize
   -----------
   procedure Finalize (Self: in out Implemented_Object) is
   begin
      if not Is_Nil(Self) then
         Self.Omniobj.all.Implobj := null ;
         Object_Destructor(Self.Omniobj.all) ;
         Self.Omniobj := null ;
      end if ;
   end ;



   -----------------------------------------------
   -----------------------------------------------
   -----------------------------------------------
   --             Omniobject                    --
   --     this type is imported from C++        --
   --   it is the equivalent of omniObject      --
   -----------------------------------------------
   -----------------------------------------------
   -----------------------------------------------


   -- C_Set_Repository_Id
   ----------------------
   procedure C_Set_Repository_Id(Self : in out Object'Class ;
                                 Repo_Id : Interfaces.C.Strings.Chars_Ptr) ;
   pragma Import (C, C_Set_Repository_Id, "setRepositoryID__14Ada_OmniObjectPCc") ;
   -- corresponds to Ada_OmniObject::setRepositoryID

   -- Set_Repository_Id
   --------------------
   procedure Set_Repository_Id(Self : in out Object'class ;
                               Repo_Id : in Corba.String) is
      C_Repo_Id : Interfaces.C.Strings.Chars_Ptr ;
   begin
      if Is_Proxy(Self) then
         Ada.Exceptions.Raise_Exception(Corba.AdaBroker_Fatal_Error'Identity,
                                        "Omniobject.Set_Repository_Id(Object'class)"
                                        & Corba.CRLF
                                        & "Cannot be called on proxy objects") ;
      end if ;
      C_Repo_Id :=
        Interfaces.C.Strings.New_String(Corba.To_Standard_String(Repo_Id)) ;
      C_Set_Repository_Id(Self, C_Repo_Id) ;
   end ;



   -- C_Get_Repository_Id
   ----------------------
   function C_Get_Repository_Id(Self : in Object'class)
                                return Interfaces.C.Strings.Chars_Ptr ;
   pragma Import (C, C_Get_Repository_Id, "getRepositoryID__14Ada_OmniObject") ;
   -- corresponds to Ada_OmniObject::getRepositoryID

   -- Get_Repository_Id
   --------------------
   function Get_Repository_Id(Self : in Object'class)
                              return Corba.String is
      C_Result : Interfaces.C.Strings.Chars_Ptr ;
   begin
      C_Result := C_Get_Repository_Id(Self) ;
      return Corba.To_Corba_String(Interfaces.C.Strings.Value(C_Result)) ;
   end ;



   -- C_Init
   ---------
   procedure C_Init (Self : in out Object'Class) ;
   pragma Import (C,C_Init,"Init__14Ada_OmniObject") ;
   -- wrapper around Ada_OmniObject function Init
   -- (see Ada_OmniObject.hh)

   -- Init
   -------
   procedure Init (Self : in out Implemented_Object ;
                   Repo_Id : in Corba.String) is
   begin
      if not Is_Nil(Self) then
         C_Init (Self.Omniobj.all) ;
         Set_Repository_Id(Self.Omniobj.all,Repo_Id) ;
      else
         Ada.Exceptions.Raise_Exception(Corba.AdaBroker_Fatal_Error'Identity,
                                        "Omniobject.Init(Implemented_Object, Corba.String"
                                        & Corba.CRLF
                                        & "Cannot be called on a nil object") ;
      end if ;
   end;




   -- Ada_To_C_Unsigned_Long
   -------------------------
   function Ada_To_C_Unsigned_Long is
     new Ada.Unchecked_Conversion (Corba.Unsigned_Long,
                                   Interfaces.C.Unsigned_Long) ;
   -- needed to change ada type Corba.Unsigned_Long
   -- into C type Interfaces.C.Unsigned_Long



   -- C_Init2
   ----------
   procedure C_Init2 (Self : in out Object'Class ;
                      RepoId : in Interfaces.C.Strings.Chars_Ptr ;
                      R : in System.Address ;
                      Key : in System.Address ;
                      Keysize : in Interfaces.C.Unsigned_Long ;
                      Profiles : in System.Address ;
                      Release : Sys_Dep.C_Boolean) ;
   pragma Import (C,C_Init2,
                  "Init__14Ada_OmniObjectPCcP4RopePUcUiPt25_CORBA_Unbounded_Sequence1ZQ23IOP13TaggedProfileb") ;
   -- wrapper around Ada_OmniObject function Init
   -- the name was changed to avoid conflict
   -- (see Ada_OmniObject.hh)

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
   begin
      return Self.Implobj = null ;
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


   -- Dispatch
   -----------
   function Dispatch(Self: in Object'Class ;
                     Orls: in Giop_S.Object ;
                     Orl_Op : in String ;
                     Orl_Response_Expected : in Boolean)
                     return Boolean is
   begin
      -- ARGUMENTS : corba.string ?? corba.boolean ??
      -- to be implemented
      -- should never be called on a proxy object
      return False ;
   end ;


   -- Duplicate
   ------------
   procedure Duplicate(Self : in Object'class) is
   begin
      null ;
      -- to be implemented
   end ;

   -- Release
   ------------
   procedure Release(Self : in Object'class) is
   begin
      null ;
      -- to be implemented
   end ;



   -- Object_Ptr_Constructor
   -------------------------
   function Object_Ptr_Constructor return Object_Ptr is
      -- to convert the system.Address to access to Object
      package Address_To_Object_ptr is
        new System.Address_To_Access_Conversions (Object) ;
      -- to convert access to object to Object_Ptr
      function To_Object_Ptr is
        new Ada.Unchecked_Conversion (Address_To_Object_ptr.Object_Pointer,
                                      Object_Ptr);
      C_Result : System.Address ;
      Result : Address_To_Object_ptr.Object_Pointer ;
   begin
      C_Result := C_Object_Ptr_Constructor ;
      Result := Address_To_Object_Ptr.To_Pointer(C_Result) ;
      return To_Object_Ptr(Result) ;
   end ;


end OmniObject ;


