-----------------------------------------------------------------------
-----------------------------------------------------------------------
----                                                               ----
----                         AdaBroker                             ----
----                                                               ----
----                    package Omniobject                         ----
----                                                               ----
----                                                               ----
----   Copyright (C) 1999 ENST                                     ----
----                                                               ----
----   This file is part of the AdaBroker library                  ----
----                                                               ----
----   The AdaBroker library is free software; you can             ----
----   redistribute it and/or modify it under the terms of the     ----
----   GNU Library General Public License as published by the      ----
----   Free Software Foundation; either version 2 of the License,  ----
----   or (at your option) any later version.                      ----
----                                                               ----
----   This library is distributed in the hope that it will be     ----
----   useful, but WITHOUT ANY WARRANTY; without even the implied  ----
----   warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR     ----
----   PURPOSE.  See the GNU Library General Public License for    ----
----   more details.                                               ----
----                                                               ----
----   You should have received a copy of the GNU Library General  ----
----   Public License along with this library; if not, write to    ----
----   the Free Software Foundation, Inc., 59 Temple Place -       ----
----   Suite 330, Boston, MA 02111-1307, USA                       ----
----                                                               ----
----                                                               ----
----                                                               ----
----   Description                                                 ----
----   -----------                                                 ----
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
----                                                               ----
----   authors : Sebastien Ponce, Fabien Azavant                   ----
----   date    : 02/28/99                                          ----
----                                                               ----
-----------------------------------------------------------------------
-----------------------------------------------------------------------

with Ada.Exceptions ;
with Ada.Unchecked_Conversion ;
with Ada.Characters.Latin_1 ;

with System ;
use type System.Address ;

with Corba ;
use type Corba.String ;
use type Corba.Unsigned_Long ;
with Omni ;

with Adabroker_Debug ; use Adabroker_Debug ;

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



   -----------------------------------------------
   ---            miscellaneous                ---
   -----------------------------------------------

   -- C_Init
   ---------
   procedure C_Init_Local_Object (Self : in out Object'Class ;
                                  Repoid : in Interfaces.C.Strings.Chars_Ptr ) ;
   pragma Import (CPP,C_Init_Local_Object,"initLocalObject__14Ada_OmniObjectPCc") ;
   -- wrapper around Ada_OmniObject::initLocalObject
   -- (see Ada_OmniObject.hh)

   -- Init_Local_Object
   --------------------
   procedure Init_Local_Object (Self : in out Implemented_Object'Class ;
                                Repo_Id : in Corba.String) is
      C_Repoid : Interfaces.C.Strings.Chars_Ptr ;
   begin
      Initialize(Implemented_Object(Self)) ;
      if not Is_Nil(Self) then
         C_Repoid := Interfaces.C.Strings.New_String(Corba.To_Standard_String(Repo_Id)) ;
         C_Init_Local_Object (Self.Omniobj.all, C_repoid) ;
         Interfaces.C.Strings.Free(C_Repoid) ;
      else
         Ada.Exceptions.Raise_Exception(Corba.AdaBroker_Fatal_Error'Identity,
                                        "Omniobject.Init(Implemented_Object, Corba.String"
                                        & Corba.CRLF
                                        & "Cannot be called on a nil object") ;
      end if ;
   end;


   -- Is_Nil
   ---------
   function Is_Nil(Self : in Implemented_Object) return Corba.Boolean is
   begin
      return Self.Omniobj = null ;
   end ;


   -- Get_Repository_Id
   --------------------
   function Get_Repository_Id(Self : in Implemented_Object)
                              return Corba.String is
   begin
      return Repository_Id ;
   end ;


   -- Hash
   -------
   function Hash(Self : in Implemented_Object ;
                 Maximum : in Corba.Unsigned_Long)
                 return Corba.Unsigned_Long is
   begin

      if Is_Nil(Self) then
         Ada.Exceptions.Raise_Exception(Constraint_Error'Identity,
                                        "Cannot call hash on a nil object") ;
      end if ;
      return Hash(Self.Omniobj.all, Maximum) ;
   end ;

   -----------------------------------------------
   --        dispatching operators              --
   -----------------------------------------------


   -- Is_A
   -------
   function Is_A(Self: in Implemented_Object ;
                 Logical_Type_Id : in Corba.String)
                 return Corba.Boolean is
   begin
      return (Repository_Id = Logical_Type_Id) ;
   end ;


   -----------------------------------------------
   --      registering into the ORB             --
   -----------------------------------------------

   -- Object_Is_Ready
   ------------------
   procedure Object_Is_Ready(Self : in Implemented_Object'Class) is
   begin
      if not Is_Nil(Self) then
         Omniobject_Is_Ready(Self.Omniobj) ;
      else
         Ada.Exceptions.Raise_Exception(Corba.Adabroker_Fatal_Error'Identity,
                                        "Omniobject.Object_Is_Ready(Implemented_Object)"
                                        & Corba.CRLF
                                        & "Cannot be called on nil object") ;
      end if ;
   end ;


   -- Dispose_Object
   -----------------
   procedure Dispose_Object(Self : in Implemented_Object'Class) is
   begin
      if not Is_Nil(Self) then
         Omniobject_Dispose(Self.Omniobj) ;
      else
         Ada.Exceptions.Raise_Exception(Corba.Adabroker_Fatal_Error'Identity,
                                        "Omniobject.Dispose_Object(Implemented_Object)"
                                        & Corba.CRLF
                                        & "Cannot be called on nil object") ;
      end if ;
   end ;



   -----------------------------------------------
   --             object <-> IOR                --
   -----------------------------------------------


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


   -----------------------------------------------
   ---        marshalling operators            ---
   -----------------------------------------------

   -- Align_Size
   -------------
   function Align_Size (Obj : in Implemented_Object_Ptr ;
                        Initial_Offset : in Corba.Unsigned_Long)
                        return Corba.Unsigned_Long is
   begin
      if Obj = null then
         -- never reached normally
         Ada.Exceptions.Raise_Exception (Corba.AdaBroker_Fatal_Error'Identity ,
                                     "Null pointer argument in function Align_Size in corba-object.") ;
      else
         -- calls the corresponding function on the underlying omniobject
         return Align_Size (Obj.all.Omniobj,Initial_Offset) ;
      end if ;
   end ;


   -- Marshall
   -----------
   procedure Marshall (Obj : in Implemented_Object_Ptr ;
                       S : in out NetBufferedStream.Object'Class) is
   begin
      if Obj = null then
         -- never reached normally
         Ada.Exceptions.Raise_Exception (Corba.AdaBroker_Fatal_Error'Identity ,
                                     "Null pointer argument in procedure Marshall in corba-object.") ;
      else
         -- calls the corresponding function on the underlying omniobject
         Marshall (Obj.all.Omniobj,S) ;
      end if ;
   end ;


   -- Marshall
   -----------
   procedure Marshall (Obj : in Implemented_Object_Ptr ;
                       S : in out MemBufferedStream.Object'Class) is
   begin
      if Obj = null then
         -- never reached normally
         Ada.Exceptions.Raise_Exception (Corba.AdaBroker_Fatal_Error'Identity ,
                                     "Null pointer argument in procedure Marshall in corba-object.") ;
      else
         -- calls the corresponding function on the underlying omniobject
         Marshall (Obj.all.Omniobj,S) ;
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

   -----------------------------------------------
   ---        marshalling operators            ---
   -----------------------------------------------

   -- Align_Size
   -------------
   function Align_Size (Obj : in Object_Ptr ;
                        Initial_Offset : in Corba.Unsigned_Long)
                        return Corba.Unsigned_Long is
      Offset : Corba.Unsigned_Long ;
   begin
      -- Alignment for type Corba.unsigned_long
      Offset := Omni.Align_To (Initial_Offset,Omni.ALIGN_4) ;
      if Obj = null then
         -- if object null, the marshall will need 12 Bytes
         return Offset + 12 ;
      else
         -- if not null, add size of unsigned_long (size of Repo_Id),
         -- size of Repo_Id itself and size of profiles
         declare
            Repo_ID : Corba.String := Get_Repository_Id (Obj.all) ;
         begin
            Offset := Offset + 4 ;
            Offset := Offset + Corba.Length (Repo_ID) + 1 ;
            Offset := Iop.Align_Size (Get_Profile_List (Obj.all),Offset) ;
            return Offset ;
         end ;
      end if ;
   end ;


   -- Marshall
   -----------
   procedure Marshall (Obj : in Object_Ptr ;
                       S : in out NetBufferedStream.Object'Class) is
   begin
      if Obj = null then
         -- if object null, marshall null object
         NetBufferedStream.Marshall (Corba.Unsigned_Long (1),S) ;
         NetBufferedStream.Marshall (Corba.Char (Ada.Characters.Latin_1.nul),S) ;
         NetBufferedStream.Marshall (Corba.Unsigned_Long (0),S) ;
      else
         -- else marshall the Repo_ID and the iopProfiles
         NetBufferedStream.Marshall (Get_Repository_Id (Obj.all),S) ;
         Iop.Marshall (Get_Profile_List (Obj.all), S) ;
      end if ;
   end ;


   -- Marshall
   -----------
   procedure Marshall (Obj : in Object_Ptr ;
                       S : in out MemBufferedStream.Object'Class) is
   begin
      if Obj = null then
         -- if object null, marshall null object
         MemBufferedStream.Marshall (Corba.Unsigned_Long (1),S) ;
         MemBufferedStream.Marshall (Corba.Char (Ada.Characters.Latin_1.nul),S) ;
         MemBufferedStream.Marshall (Corba.Unsigned_Long (0),S) ;
      else
         -- else marshall the Repo_ID and the iopProfiles
         MemBufferedStream.Marshall (Get_Repository_Id (Obj.all),S) ;
         Iop.Marshall (Get_Profile_List (Obj.all), S) ;
      end if ;
   end ;


   -----------------------------------------------
   --        constructors and destrucotrs       --
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
      C_Mdr : Interfaces.C.Strings.Chars_Ptr ;
      C_R : Sys_Dep.C_Boolean ;
      C_Profiles : System.Address ;
      C_Result : System.Address ;
   begin
      C_Mdr := Interfaces.C.Strings.New_String(Corba.To_Standard_String(Most_Derived_Repoid)) ;
      -- never deallocatd, it will be stored in the object
      C_R := Sys_Dep.Boolean_Ada_To_C(Release) ;
      C_Profiles := System.Address(Profiles) ;

      C_Result := C_Create_Omniobject(C_Mdr, C_Profiles, C_R) ;
      return To_Object_Ptr(Address_To_Object.To_Pointer(C_Result)) ;
   end ;


   -- C_Omniobject_Duplicate
   -------------------------
   function C_Omniobject_Duplicate(Self : in System.Address) return System.Address ;
   pragma Import(CPP, C_Omniobject_Duplicate, "objectDuplicate__14Ada_OmniObjectP14Ada_OmniObject") ;
   -- calls Ada_OmniObject::objectDuplicate


   -- Omniobject_Duplicate
   -----------------------
   function Omniobject_Duplicate(Self : in Object_Ptr) return Object_Ptr is
      C_Result : System.Address ;
      C_Arg : System.Address ;
   begin
      C_Arg := Address_To_Object.To_Address(From_Object_Ptr(Self)) ;
      C_Result := C_Omniobject_Duplicate(C_Arg) ;
      return To_Object_Ptr(Address_To_Object.To_Pointer(C_Result)) ;
   end ;


   -- C_Omniobject_Destructor
   --------------------------
   procedure C_Omniobject_Destructor(Self : in System.Address) ;
   pragma Import (CPP,C_Omniobject_Destructor,
                  "Destructor__14Ada_OmniObjectP14Ada_OmniObject") ;
   -- calls Ada_OmniObject::Destructor


   -- Omniobject_Destructor
   ------------------------
   procedure Omniobject_Destructor(Self : in Object_Ptr) is
   begin
      C_Omniobject_Destructor(Address_To_Object.To_Address(From_Object_Ptr(Self))) ;
   end ;


   -----------------------------------------------
   --      registering into the ORB             --
   -----------------------------------------------

   -- C_Omniobject_Is_Ready
   ------------------------
   procedure C_Omniobject_Is_Ready(Self : in System.Address) ;
   pragma Import (CPP,C_Omniobject_Is_Ready,"objectIsReady__14Ada_OmniObject") ;
   -- calls Ada_OmniObject::objectIsReady


   -- Omniobject_Is_Ready
   ----------------------
   procedure Omniobject_Is_Ready(Self : in Object_Ptr) is
   begin
      C_Omniobject_Is_Ready(Address_To_Object.To_Address(From_Object_Ptr(Self))) ;
   end ;

   -- C_Omniobject_Dispose
   -----------------------
   procedure C_Omniobject_Dispose(Self : in System.Address) ;
   pragma Import(CPP, C_Omniobject_Dispose, "disposeObject__14Ada_OmniObject") ;
   -- calls Ada_OmniObject::disposeObject


   -- Omniobject_Dispose
   ---------------------
   procedure Omniobject_Dispose(Self : in Object_Ptr) is
   begin
      C_Omniobject_Is_Ready(Address_To_Object.To_Address(From_Object_Ptr(Self))) ;
   end ;


   -----------------------------------------------
   --             object <-> IOR                --
   -----------------------------------------------

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
      C_Repoid : Interfaces.C.Strings.Chars_Ptr ;
      C_Result : System.Address ;
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
         return To_Object_Ptr(Address_To_Object.To_Pointer(C_Result)) ;
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
      C_Obj_ptr : System.Address ;
      C_Result : Interfaces.C.Strings.Chars_Ptr ;
   begin
      C_Obj_ptr := Address_To_Object.To_Address(From_Object_Ptr(Obj_Ptr)) ;
      C_Result := C_Object_To_String(C_Obj_ptr) ;
      return Corba.To_Corba_String(Interfaces.C.Strings.Value(C_Result)) ;
   end ;


   -----------------------------------------------
   --          miscellaneous                    --
   -----------------------------------------------

   -- C_Hash
   ---------
   function C_Hash(Self : in Object'Class ;
                   Maximum : in Interfaces.C.Unsigned_Long)
                   return Interfaces.C.Unsigned_Long ;
   pragma Import (CPP, C_Hash, "hash__14Ada_OmniObjectUl") ;
   -- calls Ada_OmniObject::hash

   -- Hash
   -------
   function Hash(Self : in Object'Class ;
                 Maximum : in Corba.Unsigned_Long)
                 return Corba.Unsigned_Long is
   begin
      return Corba.Unsigned_Long(C_Hash(Self, Interfaces.C.Unsigned_Long(Maximum))) ;
   end ;

   -- C_Non_Existent
   -----------------
   function C_Non_Existent(Self : in Object'Class) return Sys_Dep.C_Boolean;
   pragma Import(CPP, C_Non_Existent, "non_existent__14Ada_OmniObject") ;
   -- calls Ada_OmniObject::non_existent

   -- Non_Existent
   ---------------
   function Non_Existent(Self : in Object'Class) return Corba.Boolean is
   begin
      return Sys_Dep.Boolean_C_To_Ada(C_Non_Existent(Self)) ;
   end ;


   -- C_Get_Rope_And_Key
   ---------------------
   procedure C_Get_Rope_And_Key (Self : in Object'Class ;
                                L : in out Omniropeandkey.Object ;
                                Success : out Sys_Dep.C_Boolean) ;
   pragma Import (CPP,
                  C_Get_Rope_And_Key,
                  "getRopeAndKey__14Ada_OmniObjectR18Ada_OmniRopeAndKeyRb") ;
   -- wrapper around  Ada_OmniObject function getRopeAndKey
   -- (see Ada_OmniObject.hh)


   -- Get_Rope_And_Key
   -------------------
   procedure Get_Rope_And_Key (Self : in Object'Class ;
                               L : in out Omniropeandkey.Object ;
                               Success : out Corba.Boolean ) is
      C_Success : Sys_Dep.C_Boolean ;
   begin
      if Is_Proxy(Self) then
         Ada.Exceptions.Raise_Exception(Corba.AdaBroker_Fatal_Error'Identity,
                                        "Omniobject.Get_Rope_And_Key cannot be called on a local object") ;
      end if ;
      -- ... calls the C function ...
      C_Get_Rope_And_Key(Self, L, C_Success) ;
      -- ... and transforms the result into an Ada type
      Success := Sys_Dep.Boolean_C_To_Ada (C_Success) ;
   end ;


   -- C_Get_Repository_Id
   ----------------------
   function C_Get_Repository_Id(Self : in Object'class)
                                return Interfaces.C.Strings.Chars_Ptr ;
   pragma Import (CPP, C_Get_Repository_Id, "getRepositoryID__14Ada_OmniObject") ;
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


   -----------------------------------------------
   --         Implemented_Object                --
   --       this is the type of local           --
   --      implementations of objects           --
   -- it is the root of all XXX.Impl.Object     --
   -----------------------------------------------
   --           PRIVATE PART                    --
   -----------------------------------------------


   -----------------------------------------------
   ---     finalization operators              ---
   -----------------------------------------------

   -- Initialize
   -------------
   procedure Initialize (Self: in out Implemented_Object) is
      type Ptr is access all Implemented_Object ;
      function To_Implemented_Object_Ptr is
        new Ada.Unchecked_Conversion (Ptr, Implemented_Object_Ptr);
      Tmp : Object_Ptr := Object_Ptr_Constructor ;
   begin
      Tmp.all.Implobj := To_Implemented_Object_Ptr(Self'Access) ;
      Self.Omniobj := Tmp ;
   end ;



   -- Adjust
   -----------
   procedure Adjust (Self: in out Implemented_Object) is
   begin
      if not Is_Nil(Self) then
         Self.Omniobj := Omniobject_Duplicate(Self.omniobj) ;
      end if ;
   end ;

   -- Finalize
   -----------
   procedure Finalize (Self: in out Implemented_Object) is
   begin
      if not Is_Nil(Self) then
         Omniobject_Dispose(Self.Omniobj) ;
         Omniobject_Destructor(Self.Omniobj) ;
         Self.Omniobj := null ;
      end if ;
   end ;


   -----------------------------------------------
   --             Omniobject                    --
   --     this type is imported from C++        --
   --   it is the equivalent of omniObject      --
   -----------------------------------------------
   --           PRIVATE PART                    --
   -----------------------------------------------


   -- Is_Proxy
   -----------
   function Is_Proxy (Self : in Object'Class)
                      return Boolean is
   begin
      return Self.Implobj = null ;
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


   -- C_Set_Rope_And_Key
   ----------------------
   procedure C_Set_Rope_And_Key (Self : in out Object'Class ;
                                 L : in out Omniropeandkey.Object ;
                                 KeepIOP : in Sys_Dep.C_Boolean) ;
   pragma Import (CPP,C_Set_Rope_And_Key,
                  "setRopeAndKey__14Ada_OmniObjectRC18Ada_OmniRopeAndKeyb") ;
   -- wrapper around  Ada_OmniObject function setRopeAndKey
   -- (see Ada_OmniObject.hh)


   -- Set_Rope_And_Key
   -------------------
   procedure Set_Rope_And_Key (Self : in out Object'Class ;
                               L : in out Omniropeandkey.Object ;
                               KeepIOP : in Boolean := True) is
      C_KeepIOP : Sys_Dep.C_Boolean ;
   begin
      -- transforms the arguments into a C type ...
      C_KeepIOP := Sys_Dep.Boolean_Ada_To_C (KeepIOP) ;
      -- ... and calls the C procedure
      C_Set_Rope_And_Key (Self,L,C_KeepIOP) ;
   end ;


   -- Dispatch
   -----------
   procedure Dispatch(Self: in Object'Class ;
                      Orls: in out Giop_S.Object ;
                      Orl_Op : in Standard.String ;
                      Orl_Response_Expected : in Corba.Boolean ;
                      Success : out Corba.Boolean) is
   begin
      pragma Debug(Output(Omniobject,
                          "Omniobject.Dispatch : about to make dispatching call")) ;
      -- check there is no error
      if Self.Implobj = null then
         Ada.Exceptions.Raise_Exception(Corba.Adabroker_Fatal_Error'Identity,
                                        "Omniobject.Dispatch should not be called on a proxy object") ;
      else
       pragma Debug(Output(Omniobject,
                          "Omniobject.Dispatch : making dispatching call")) ;
        Dispatch(Self.Implobj,
                  Orls,
                  Orl_Op,
                  Orl_Response_Expected,
                  success) ;
      end if ;
   end ;


   -- C_Dispatch
   -------------
   procedure C_Dispatch (Self : in Object'Class ;
                         Orls : in out Giop_S.Object ;
                         Orl_Op : in Interfaces.C.Strings.Chars_Ptr ;
                         Orl_Response_Expected : in Sys_Dep.C_Boolean ;
                         Success : out Sys_Dep.C_Boolean) is
      Ada_Orl_Op : Standard.String := Interfaces.C.Strings.Value(Orl_OP) ;
      Ada_Orl_Response_Expected : Corba.Boolean ;
      Ada_Success : Corba.Boolean ;
   begin
      -- transforms the arguments in a Ada type, ...
      Ada_Orl_Response_Expected := Sys_Dep.Boolean_C_To_Ada (Orl_Response_Expected) ;
      -- ... calls the ada function ...
      Dispatch (Self,
                Orls,
                Ada_Orl_Op,
                Ada_Orl_Response_Expected,
                Ada_Success) ;
      -- ... and transforms the result into a C type
      Success := Sys_Dep.Boolean_Ada_To_C (Ada_Success) ;
   end ;


    -- C_Is_A
   ----------
   function C_Is_A(Self : in Object'Class ;
                   RepoId : in Interfaces.C.Strings.Chars_Ptr)
                   return  Sys_Dep.C_Boolean is
      Rep : Corba.String ;
   begin
      Rep := Corba.To_Corba_String(Interfaces.C.Strings.Value(RepoId)) ;
      return Sys_Dep.Boolean_Ada_To_C(Is_A(Self.Implobj.all, Rep)) ;
   end ;


   -----------------------------------------------
   ---         memory handling                 ---
   -----------------------------------------------


   -- C_Object_Ptr_Constructor
   ----------------------------
   function C_Object_Ptr_Constructor return System.Address ;
   pragma Import (CPP,C_Object_Ptr_Constructor,"Constructor__14Ada_OmniObject") ;
   -- This is a workaround for gnat 3.11p
   -- we cannot write
   -- toto : Object_Ptr := new Object
   -- we have to call the C++ constructor to create objects

   -- Object_Ptr_Constructor
   -------------------------
   function Object_Ptr_Constructor return Object_Ptr is
      C_Result : System.Address ;
   begin
      C_Result := C_Object_Ptr_Constructor ;
      return To_Object_Ptr(Address_To_Object.To_Pointer(C_Result)) ;
   end ;


end OmniObject ;
