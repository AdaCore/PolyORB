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
-----                                                               ----
----                                                               ----
----   authors : Sebastien Ponce, Fabien Azavant                   ----
----   date    : 02/28/99                                          ----
----                                                               ----
-----------------------------------------------------------------------
-----------------------------------------------------------------------


with Ada.Finalization ;
with Interfaces.CPP ;
with Interfaces.C.Strings ;
with System ;

with Corba ;
with Giop_S ;
with Sys_Dep ;
with Iop ;
with Rope ;
with OmniRopeAndKey ;
with Netbufferedstream ;
with Membufferedstream ;

package OmniObject is

   -----------------------------------------------
   --         Implemented_Object                --
   --       this is the type of local           --
   --      implementations of objects           --
   -- it is the root of all XXX.Impl.Object     --
   -----------------------------------------------
   --            TYPE DEFINITION                --
   -----------------------------------------------

   type Implemented_Object is tagged private ;

   type Implemented_Object_Ptr is access all Implemented_Object'Class ;


   -----------------------------------------------
   --             Omniobject                    --
   --     this type is imported from C++        --
   --   it is the equivalent of omniObject      --
   -----------------------------------------------
   --            TYPE DEFINITION                --
   -----------------------------------------------
   type Object is tagged limited record
      Implobj : Implemented_Object_Ptr  := null ;
      -- Ada field
      -- the pointer implobj is null for proxy object
      C_Object : System.Address ;
      -- C field : Pointer on the underlying omniObject_C2Ada object
      Init_Ok : Sys_Dep.C_Boolean ;
      -- C field : state of the object (initialized or not)
      Table : Interfaces.CPP.Vtable_Ptr ;
      -- Ada field : needed to interface C++ and Ada
   end record ;
   pragma CPP_Class (Object);
   pragma CPP_Vtable (Object,Table,2);
   -- this type is both a C and an Ada class
   -- it is is wrapped around Ada_OmniObject
   -- (see Ada_OmniObject.hh)

   type Object_Ptr is access all Object'Class ;


   -----------------------------------------------
   --         Implemented_Object                --
   --       this is the type of local           --
   --      implementations of objects           --
   -- it is the root of all XXX.Impl.Object     --
   -----------------------------------------------
   --        SUBPROGRAMS DECLARATION            --
   -----------------------------------------------

   procedure Init (Self : in out Implemented_Object ;
                   Repo_Id : in Corba.String) ;
   -- calls the C++ Init to set the init_ok boolean to true
   -- and sets the repoID of this object


   function Is_Nil(Self : in Implemented_Object) return Corba.Boolean ;
   function Is_Null(Self: in Implemented_Object) return Corba.Boolean renames Is_Nil;
   -- returns True if this is a nil object


   function Get_Repository_Id(Self : in Implemented_Object)
                              return Corba.String ;


    Repository_Id : Corba.String
     := Corba.To_Corba_String("IDL:omg.org/CORBA/Object:1.0") ;


    function Is_A(Self: in Implemented_Object ;
                 Logical_Type_Id : in Corba.String)
                 return Corba.Boolean ;
   -- returns true if this object is of this Logical_Type_Id
   -- ( where Logical_Type_Id is a Repository_Id )
   -- or one of its descendants


   procedure Object_Is_Ready(Self : in Implemented_Object'Class) ;
   -- calls the C++ function omni::objectIsReady
   -- has to be done when an object has been created
   -- to register it into the ORB
   -- (as a local object)
   -- BEWARE : MUST BE CALLED ONLY ONCE FOR EACH OBJECT


   function Object_To_String(Self : in Implemented_Object'Class)
                             return Corba.String ;
   -- return the IOR corresponding to this object


   function Dispatch (Self : in Implemented_Object ;
                      Orls : in Giop_S.Object ;
                      Orl_Op : in Standard.String ;
                      Orl_Response_Expected : in Corba.Boolean)
                      return Corba.Boolean ;
   -- this function is called by the C one
   -- It is implemented in the sub-classes of omniObject
   -- this function on this object should never be called


   function Align_Size (Obj : in Implemented_Object_Ptr ;
                        Initial_Offset : in Corba.Unsigned_Long)
                        return Corba.Unsigned_Long ;
   -- This function computes the size needed to marshall the object obj


   procedure Marshall (Obj : in Implemented_Object_Ptr ;
                       S : in out NetBufferedStream.Object) ;
   -- This procedure marshalls the object Obj into the stream S


   procedure Marshall (Obj : in Implemented_Object_Ptr ;
                       S : in out MemBufferedStream.Object) ;
   -- This procedure marshalls the object Obj into the stream S


   -----------------------------------------------
   --             Omniobject                    --
   --     this type is imported from C++        --
   --   it is the equivalent of omniObject      --
   -----------------------------------------------
   --        SUBPROGRAMS DECLARATION            --
   -----------------------------------------------


   function Align_Size (Obj : in Object_Ptr ;
                        Initial_Offset : in Corba.Unsigned_Long)
                        return Corba.Unsigned_Long ;
   -- This function computes the size needed to marshall the object obj


   procedure Marshall (Obj : in Object_Ptr ;
                       S : in out NetBufferedStream.Object) ;
   -- This procedure marshalls the object Obj into the stream S


   procedure Marshall (Obj : in Object_Ptr ;
                       S : in out MemBufferedStream.Object) ;
   -- This procedure marshalls the object Obj into the stream S


   function Create_Omniobject(Most_Derived_Repoid : in Corba.String ;
                              Profiles : in Iop.Tagged_Profile_List ;
                              Release : in Corba.Boolean)
                              return Object_Ptr ;
   -- this function is called by corba.object.create_ref,
   -- which is called by corba.object.unmarshall
   -- We use this when we want to unmarshall an object out of
   -- a bufferedstream, and to create a Corba.object.ref'class.


   procedure Duplicate(Self : in Object'Class) ;
   -- calls the C++ equivalent :
   -- omni::objectDuplicate(omniObject*)
   -- it increments the reference count by one


   procedure Release(Self : in Object'Class) ;
   -- calls the C++ equivalent :
   -- omni::objectRelease(omniObject*)
   -- it decrements the reference count by one
   -- and releases the resources if it comes to 0


   procedure Omniobject_Is_Ready(Self : in Object'Class) ;
   pragma Import (C,Omniobject_Is_Ready,"objectIsReady__4omniP10omniObject") ;
   -- corresponds to omni::objectIsReady
   -- objectRef.cc L 230
   -- only called by Object_Is_Ready(Implemented_Object)
   -- or Object_Is_Ready(Corba.Object.Ref)


   procedure Get_Rope_And_Key (Self : in Object'Class ;
                              L : in out Omniropeandkey.Object ;
                              Success : out Boolean ) ;
   -- returns the rope and key for this omniobject
   -- if it is a proxy object


   function Get_Repository_Id(Self : in Object'class)
                              return Corba.String ;
   -- returns the repository_id for this object


   function String_To_Object(RepoId : in Corba.String)
                             return Object_Ptr ;
   -- this function trys to create the Object
   -- corresponding to this resitory_id,
   -- returns null on failure


   function Object_To_String(Obj_ptr : in Object_Ptr) return Corba.String ;
   -- returns the IOR for this object
   -- Obj can be null


   function Get_Profile_List (Self : in Object'Class)
                              return Iop.Tagged_Profile_List ;
   -- returns the Profile list of an object

private

   -----------------------------------------------
   --         Implemented_Object                --
   --       this is the type of local           --
   --      implementations of objects           --
   -- it is the root of all XXX.Impl.Object     --
   -----------------------------------------------
   --           PRIVATE PART                    --
   -----------------------------------------------


  type Implemented_Object is new Ada.Finalization.Controlled with record
      Omniobj : Object_Ptr ;
   end record ;


   procedure Initialize (Self: in out Implemented_Object);
   -- create the underlying Omniobject
   -- both objects point on one another

   procedure Adjust (Self: in out Implemented_Object) ;
   -- create the underlying Omniobject
   -- both objects point on one another
   -- sets the repository ID


   procedure Finalize (Self: in out Implemented_Object);
   -- release the underlying omniobject


   -----------------------------------------------
   --             Omniobject                    --
   --     this type is imported from C++        --
   --   it is the equivalent of omniObject      --
   -----------------------------------------------
   --           PRIVATE PART                    --
   -----------------------------------------------


   function Is_Proxy (Self : in Object'Class)
                      return Boolean ;
   -- returns true if this is a proxy object
   -- there is no need to call the C++ function
   -- To Know It Because We Have The Information in Ada


   procedure Set_Repository_Id(Self : in out Object'class ;
                               Repo_Id : in Corba.String) ;
   -- sets the reopsitory ID for this object
   -- it is called by Init(Omniobject.Implemented_Object)

   procedure Set_Rope_And_Key (Self : in out Object'Class ;
                               L : in Omniropeandkey.Object ;
                               KeepIOP : in Boolean) ;
   -- sets the rope and key for this object


   function C_Dispatch (Self : in Object'Class ;
                        Orls : in System.Address ;
                        Orl_Op : in Interfaces.C.Strings.Chars_Ptr ;
                        Orl_Response_Expected : in Sys_Dep.C_Boolean)
                        return Sys_Dep.C_Boolean ;
   pragma Export (CPP,C_Dispatch,
                  "dispatch__14Ada_OmniObjectR6GIOP_SPCcb");
   -- wrapper around Ada_OmniObject function dispatch
   -- (see Ada_OmniObject.hh)
   -- This function is implemented in Ada and exported to C
   -- it calls th Ada function Dispatch


   function  C_Is_A(Self : in Object'Class ;
                   RepoId : in Interfaces.C.Strings.Chars_Ptr)
                   return  Sys_Dep.C_Boolean ;
   pragma Export(CPP, C_Is_A, "Ada_Is_A__14Ada_OmniObjectPCc") ;
   -- returns true if self is in repoID,
   -- or one of its descendant


   procedure Assert_Object_Existent (Self : in Object'Class) ;
   pragma Import (CPP,Assert_Object_Existent,
                  "assertObjectExistent__10omniObject");
   -- wrapper around  Ada_OmniObject function assertObjectExistent
   -- (see Ada_OmniObject.hh)
   -- no Ada equivalent since there is no arguments

   -----------------------------------------------
   ---         memory handling                 ---
   -----------------------------------------------

   function Constructor return Object'Class;
   pragma Import (CPP,Constructor,"__14Ada_OmniObject");
   pragma CPP_Constructor (Constructor);
   -- wrapped around the C constructor of Ada_OmniObject


   function Object_Ptr_Constructor return Object_Ptr ;
   -- This is a workaround for gnat 3.11p
   -- we cannot write
   -- toto : Object_Ptr := new Object
   -- we have to call the C++ constructor to create objects


   procedure Object_Destructor(Self : in out Object'Class) ;
   pragma Import (CPP,Object_Destructor,
                  "Destructor__14Ada_OmniObjectP14Ada_OmniObject") ;
   -- C++ destructor of Ada_omniObject


end OmniObject ;










