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

with System ;
with System.Address_To_Access_Conversions ;
use type System.Address ;

with Corba ;
use type Corba.String ;

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


   -- Dispatch
   -----------
   function Dispatch (Self : in Implemented_Object ;
                      Orls : in Giop_S.Object ;
                      Orl_Op : in Standard.String ;
                      Orl_Response_Expected : in Corba.Boolean)
                      return Corba.Boolean is
   begin
      Ada.Exceptions.Raise_Exception(Corba.Adabroker_Fatal_Error'Identity,
                                     "Omniobject.Dispatch(Implemented_Object)"
                                     & Corba.CRLF
                                     & "should never be called on an Implemented_Object") ;
      return False ;
      -- to please the compiler !!
   end ;


    -- Get_Profile_List
   -------------------
   function Get_Profile_List (Obj : in Implemented_Object)
                              return Iop.Tagged_Profile_List is
   begin
      return Omniobject.Get_Profile_List (Obj.Omniobj.all) ;
      -- calls the corresponding function on the underlying omniobject
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
         Release(Self.Omniobj.all) ;
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


   -- C_Create_Omniobject
   ----------------------
   function C_Create_Omniobject(Most_Derived_Repoid : in Interfaces.C.Strings.Chars_ptr ;
                                Profiles : in System.Address ;
                                Release : in Sys_Dep.C_Boolean)
                                return System.Address ;
   pragma Import(CPP, C_Create_Omniobject, "ada_create_objref__14Ada_OmniObjectPCcPt25_CORBA_Unbounded_Sequence1ZQ23IOP13TaggedProfileb") ;
   -- corresponds to  Ada_OmniObject::ada_create_objref
   -- see Ada_OmniObject.hh

    -- Create_Omniobject
   --------------------
   function Create_Omniobject(Most_Derived_Repoid : in Corba.String ;
                              Profiles : in Iop.Tagged_Profile_List ;
                              Release : in Corba.Boolean)
                              return Object_Ptr is
      package Address_To_Object_ptr is
        new System.Address_To_Access_Conversions (Object) ;
      -- to convert access to object to Object_Ptr
      function To_Object_Ptr is
        new Ada.Unchecked_Conversion (Address_To_Object_ptr.Object_Pointer,
                                      Object_Ptr);
      C_Mdr : Interfaces.C.Strings.Chars_Ptr ;
      C_R : Sys_Dep.C_Boolean ;
      C_Profiles : System.Address ;
      C_Result : System.Address ;
      Result : Address_To_Object_Ptr.Object_Pointer ;
   begin
      C_Mdr := Interfaces.C.Strings.New_String(Corba.To_Standard_String(Most_Derived_Repoid)) ;
      -- never deallocatd, it will be stored in the object
      C_R := Sys_Dep.Boolean_Ada_To_C(Release) ;
      C_Profiles := System.Address(Profiles) ;

      C_Result := C_Create_Omniobject(C_Mdr, C_Profiles, C_R) ;
      Result := Address_To_Object_Ptr.To_Pointer(C_Result) ;
      return To_Object_Ptr(Result) ;
   end ;



   -- C_Set_Repository_Id
   ----------------------
   procedure C_Set_Repository_Id(Self : in out Object'Class ;
                                 Repo_Id : Interfaces.C.Strings.Chars_Ptr) ;
   pragma Import (CPP, C_Set_Repository_Id, "setRepositoryID__14Ada_OmniObjectPCc") ;
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
      C_Repo_Id := Interfaces.C.Strings.New_String(Corba.To_Standard_String(Repo_Id)) ;
                -- desallocation in 2 lines
      C_Set_Repository_Id(Self, C_Repo_Id) ;
                -- desallocation of C_Repo_Id
      Interfaces.C.Strings.Free (C_Repo_Id) ;
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



   -- C_String_To_Object
   ---------------------
   function C_String_To_Object(RepoId : in Interfaces.C.Strings.Chars_Ptr)
                               return System.Address ;
   pragma Import (CPP, C_String_To_Object, "string_to_ada_object__14Ada_OmniObjectPCc") ;
   -- corresponds to Ada_OmniObject::string_to_ada_object

   -- String_To_Object
   -------------------
   function String_To_Object(RepoId : in Corba.String)
                             return Object_Ptr is
      package Address_To_Object_ptr is
        new System.Address_To_Access_Conversions (Object) ;
      -- to convert access to object to Object_Ptr
      function To_Object_Ptr is
        new Ada.Unchecked_Conversion (Address_To_Object_ptr.Object_Pointer,
                                      Object_Ptr);
      C_Repoid : Interfaces.C.Strings.Chars_Ptr ;
      C_Result : System.Address ;
      Result : Address_To_Object_ptr.Object_Pointer ;
   begin
      -- transform arguments into C types ...
      C_Repoid := Interfaces.C.Strings.New_String(Corba.To_Standard_String(Repoid)) ;
      -- call C function
      C_Result := C_String_To_Object(C_Repoid) ;
      -- free arguments
      Interfaces.C.Strings.Free(C_Repoid) ;
      -- transform result
      if  C_Result = System.Null_Address then
         return null ;
      else
         Result := Address_To_Object_Ptr.To_Pointer(C_Result) ;
         return To_Object_Ptr(Result) ;
      end if ;
   end ;


   -- C_Object_To_String
   ---------------------
   function C_Object_To_String(Obj : in System.Address)
                               return Interfaces.C.Strings.Chars_Ptr ;
   pragma Import(CPP, C_Object_To_String, "ada_object_to_string__14Ada_OmniObjectP14Ada_OmniObject") ;
   -- corresponds to ada_object_to_string


   -- Object_To_String
   -------------------
   function Object_To_String(Obj_ptr : in Object_Ptr) return Corba.String is
      package A2a is
        new System.Address_To_Access_Conversions (Object) ;
      function Uc is
        new Ada.Unchecked_Conversion (Object_ptr,
                                      A2a.Object_Pointer) ;
      C_Obj_ptr : System.Address ;
      C_Result : Interfaces.C.Strings.Chars_Ptr ;
   begin
      C_Obj_ptr := A2a.To_Address(Uc(Obj_Ptr)) ;
      C_Result := C_Object_To_String(C_Obj_ptr) ;
      return Corba.To_Corba_String(Interfaces.C.Strings.Value(C_Result)) ;
   end ;


   -- Ada_To_C_Unsigned_Long
   -------------------------
   function Ada_To_C_Unsigned_Long is
     new Ada.Unchecked_Conversion (Corba.Unsigned_Long,
                                   Interfaces.C.Unsigned_Long) ;
   -- needed to change ada type Corba.Unsigned_Long
   -- into C type Interfaces.C.Unsigned_Long


   -- Object_To_String
   -------------------
   function Object_To_String (Self : in Implemented_Object'class)
                              return CORBA.String is
   begin
      if Is_Nil(Self) then
         return Object_To_String(null) ;
      else
         return Object_To_String(Self.Omniobj) ;
      end if ;
   end ;


   -- C_Get_Profile_List
   ---------------------
   function C_Get_Profile_List (Self : in Object'Class)
                                return System.Address ;
   pragma Import (CPP,C_Get_Profile_List,"iopProfiles__14Ada_OmniObject") ;
   -- returns the Profile list of an object
   -- wrapper around C function Ada_OmniObject::iopProfiles()
   -- (see Ada_OmniObject.hh)

   -- Get_Profile_List
   -------------------
   function Get_Profile_List (Self : in Object'Class)
                              return Iop.Tagged_Profile_List is
      result : System.Address ;
   begin
      -- calls the C function ...
      Result := C_Get_Profile_List (Self) ;
      -- ... and transforms the result in an Ada type
      return Iop.Tagged_Profile_List (Result) ;
   end ;


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
                 -- desallocation in a few lines
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
                 -- desallocation of C_RepoID
      Interfaces.C.Strings.Free (C_RepoID) ;
   end;



  -- Object_Is_Ready
   ------------------
   procedure Object_Is_Ready(Self : in Implemented_Object'Class) is
   begin
      if not Is_Nil(Self) then
         Omniobject_Is_Ready(Self.Omniobj.all) ;
      else
         Ada.Exceptions.Raise_Exception(Corba.Adabroker_Fatal_Error'Identity,
                                        "Omniobject.Object_Is_Ready(Implemented_Object)"
                                        & Corba.CRLF
                                        & "Cannot be called on nil object") ;
      end if ;
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


   -- C_Get_Rope_And_Key
   ---------------------
   procedure C_Get_Rope_And_Key (Self : in Object'Class ;
                                L : in out System.Address ;
                                Success : out Sys_Dep.C_Boolean) ;
   pragma Import (CPP,
                  C_Get_Rope_And_Key,
                  "getRopeAndKey__C10omniObjectR14omniRopeAndKey") ;
   -- wrapper around  Ada_OmniObject function getRopeAndKey
   -- (see Ada_OmniObject.hh)

   -- Get_Rope_And_Key
   -------------------
   procedure Get_Rope_And_Key (Self : in Object'Class ;
                               L : in out Omniropeandkey.Object ;
                               Success : out Corba.Boolean ) is
      C_L : System.Address;
      C_Success : Sys_Dep.C_Boolean ;
   begin
      if Is_Proxy(Self) then
         Ada.Exceptions.Raise_Exception(Corba.AdaBroker_Fatal_Error'Identity,
                                        "Omniobject.Get_Rope_And_Key cannot be called on a local object") ;
      end if ;
      -- transforms the arguments in a C type ...
      C_L := L'Address ;
      -- ... calls the C function ...
      C_Get_Rope_And_Key(Self, C_L, C_Success) ;
      -- ... and transforms the result into an Ada type
      Success := Sys_Dep.Boolean_C_To_Ada (C_Success) ;
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


   -- Dispatch
   -----------
   function Dispatch(Self: in Object'Class ;
                     Orls: in Giop_S.Object ;
                     Orl_Op : in Standard.String ;
                     Orl_Response_Expected : in Corba.Boolean)
                     return Corba.Boolean is
   begin
      -- check there is no error
      if Self.Implobj = null then
         Ada.Exceptions.Raise_Exception(Corba.Adabroker_Fatal_Error'Identity,
                                        "Omniobject.Dispatch should not be called on a proxy object") ;
      else
         return Dispatch(Self.Implobj.all,
                         Orls,
                         Orl_Op,
                         Orl_Response_Expected) ;
      end if ;
   end ;

   -- C_Dispatch
   -------------
   function C_Dispatch (Self : in Object'Class ;
                        Orls : in System.Address ;
                        Orl_Op : in Interfaces.C.Strings.Chars_Ptr ;
                        Orl_Response_Expected : in Sys_Dep.C_Boolean)
                        return Sys_Dep.C_Boolean is
      Ada_Orls_Ptr : Address_To_Giop_S.Object_Pointer ;
      Ada_Orl_Op : Standard.String := Interfaces.C.Strings.Value(Orl_OP) ;
      Ada_Orl_Response_Expected : Corba.Boolean ;
      Ada_Result : Corba.Boolean ;
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



   -- Get_Repository_Id
   --------------------
   function Get_Repository_Id(Self : in Implemented_Object)
                              return Corba.String is
   begin
      return Repository_Id ;
   end ;


   -- Is_A
   -------
   function Is_A(Self: in Implemented_Object ;
                 Logical_Type_Id : in Corba.String)
                 return Corba.Boolean is
   begin
      return (Repository_Id = Logical_Type_Id) ;
   end ;

    -- C_Is_A
   ---------
   function C_Is_A(Self : in Object'Class ;
                   RepoId : in Interfaces.C.Strings.Chars_Ptr)
                   return  Sys_Dep.C_Boolean is
      Rep : Corba.String ;
   begin
      Rep := Corba.To_Corba_String(Interfaces.C.Strings.Value(RepoId)) ;
      return Sys_Dep.Boolean_Ada_To_C(Is_A(Self.Implobj.all, Rep)) ;
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


