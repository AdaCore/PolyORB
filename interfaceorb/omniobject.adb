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
   --         Implemented_Object                --
   --       this is the type of local           --
   --      implementations of objects           --
   -- it is the root of all XXX.Impl.Object     --
   -----------------------------------------------

   -- Is_Nil
   ---------
   function Is_Nil(Self : in Implemented_Object) return Corba.Boolean is
   begin
      if Self.Omniobj = null then
         return True ;
      else return Is_Nil(Self.Omniobj.all) ;
      end if ;
   end ;



   -- Set_Repository_Id
   --------------------
   procedure Set_Repository_Id(Self : in out Implemented_Object ;
                               Repo_Id : in Corba.String) is
      Ex_Mb : Corba.System_Exception_Members := (0, Corba.COMPLETED_NO) ;
   begin
      if Is_Nil(Self) then
         Corba.Raise_Corba_Exception(Corba.Inv_Objref'Identity, Ex_Mb) ;
      end if ;

      if Is_Proxy(Self.Omniobj.all) then
         Corba.Raise_Corba_Exception(Corba.Inv_Objref'Identity, Ex_mb) ;
      end if ;

      Set_Repository_Id(Self.Omniobj.all, Repo_Id) ;

   end ;


   -- Initialize
   -------------
   procedure Initialize (Self: in out Implemented_Object) is
      type Ptr is access all Implemented_Object ;
      function To_Implemented_Object_Object_Ptr is
        new Ada.Unchecked_Conversion (Ptr, Implemented_Object_Ptr);
   begin
      Self.Omniobj := new Object ;
      Self.Omniobj.all.Implobj := To_Implemented_Object_Object_Ptr(Self'Access) ;
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
            Set_Repository_Id(Self, RepoID) ;
         end ;
      end if ;
   end ;

   -- Finalize
   -----------
   procedure Finalize (Self: in out Implemented_Object) is
   begin
      if not Is_Nil(Self) then
         Self.Omniobj.Implobj := null ;
         Release(Self.Omniobj.all) ;
         Self.Omniobj := null ;
      end if ;
   end ;



   -----------------------------------------------
   --             Omniobject                    --
   --     this type is imported from C++        --
   --   it is the equivalent of omniObject      --
   -----------------------------------------------


    -- Is_Nil
   ---------
   function Is_Nil(Self : in Object'class) return Corba.Boolean is
   begin
      return True ;
      -- call the C++ is_nil
   end ;

   -- Set_Repository_Id
   --------------------
   procedure Set_Repository_Id(Self : in out Object'class ;
                               Repo_Id : in Corba.String) is
   begin
      -- calls PR_IRRepositoryId in C++
      null ;
   end ;

   -- Get_Repository_Id
   --------------------
   function Get_Repository_Id(Self : in Object'class)
                              return Corba.String is
   begin
      -- calls NP_IRrepositoryId in C++
      return Corba.To_Corba_String("") ;
   end ;


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



end OmniObject ;


