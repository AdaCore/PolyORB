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


with Ada.Unchecked_Conversion ;
with Ada.Finalization ;
with Interfaces.CPP ;
with Interfaces.C.Strings ;
with System ;
with System.Address_To_Access_Conversions ;

with Corba ;
with Giop_S ;
with Sys_Dep ;
with Iop ;
with Rope ;
with OmniRopeAndKey ;
with Netbufferedstream ;
with Membufferedstream ;

with Adabroker_Debug ;
pragma Elaborate(Adabroker_Debug) ;

package OmniObject is

   Omniobject : constant Boolean := Adabroker_Debug.Is_Active("omniobject") ;
   Omni_Fin : constant Boolean := Adabroker_Debug.Is_Active("omniobject:finalization") ;

   -----------------------------------------------
   --         Implemented_Object                --
   --       this is the type of local           --
   --      implementations of objects           --
   -- it is the root of all XXX.Impl.Object     --
   -----------------------------------------------
   --            TYPE DEFINITION                --
   -----------------------------------------------

   type Implemented_Object is abstract tagged limited private ;

   type Implemented_Object_Ptr is access all Implemented_Object'Class ;


   type Dispatch_Procedure is access
     procedure (Self : Implemented_Object_Ptr ;
                Orls : in out Giop_S.Object ;
                Orl_Op : in Standard.String ;
                Orl_Response_Expected : in Corba.Boolean ;
                Success : out Corba.Boolean) ;
   -- this type is used to make the dispatchnig call.
   -- this type is made to handle dispatchnig calls
   -- from the ORB. This procedure cannot be made a primitive
   -- of Implemented_Object, therwise it would have to
   -- for each descendant in the user's code.

   type Is_A_Function is access
     function (Repoid : in Corba.String ) return Corba.Boolean ;
   -- this type is used to make the dispatchnig call.
   -- this type is made to handle dispatchnig calls
   -- from the ORB. This procedure cannot be made a primitive
   -- of Implemented_Object, therwise it would have to
   -- for each descendant in the user's code.


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

   -----------------------------------------------
   ---            miscellaneous                ---
   -----------------------------------------------

   procedure Init_Local_Object (Self : in out Implemented_Object ;
                                Repo_Id : in Corba.String ;
                                Disp : in Dispatch_Procedure ;
                                Isa : in Is_A_Function ) ;
   -- calls the C++ Init to set the init_ok boolean to true
   -- and sets the repoID of this object. It also sets
   -- the dispatch procedure associated with this object


   function Is_Nil(Self : in Implemented_Object) return Corba.Boolean ;
   function Is_Null(Self: in Implemented_Object) return Corba.Boolean renames Is_Nil;
   -- returns True if this is a nil object


   function Get_Repository_Id(Self : in Implemented_Object)
                              return Corba.String ;
   -- return the repository_id of the C++ object


   Repository_Id : Corba.String
     := Corba.To_Corba_String(Standard.String'("IDL:omg.org/CORBA/Object:1.0")) ;

   function Hash(Self : in Implemented_Object ;
                 Maximum : in Corba.Unsigned_Long) return Corba.Unsigned_Long ;
   -- returns a hash value for this object

   function Get_Object_Ptr(Self : in Implemented_Object) return Object_Ptr;
   -- returns its underlying Object

   -----------------------------------------------
   --      registering into the ORB             --
   -----------------------------------------------

   procedure Object_Is_Ready(Self : in Implemented_Object'Class) ;
   -- tells the ORB that this object is ready to accept connections
   -- it has to be done once (and only once) for each local object
   -- this function *must not* be called directly by an implmentation.
   -- Call the Boa.Object_Is_Ready instead. There is no difference in
   -- this version of AdaBroker since there is only one BOA, but
   -- there may be in the future.


   procedure Dispose_Object(Self : in Implemented_Object'Class) ;
   -- Releases all the ressources associated to a local object
   -- it is done automatically at the end of the scope, but can
   -- be done prematuraly by the invokation of this procedure.
   -- Once this procedure has been called, the Implemented_Object
   -- becomes a null object.


   -----------------------------------------------
   --             object <-> IOR                --
   -----------------------------------------------

   function Object_To_String(Self : in Implemented_Object'Class)
                             return Corba.String ;
   -- return the IOR corresponding to this object



   -----------------------------------------------
   --           marshalling operators           --
   -----------------------------------------------

   function Align_Size (Obj : in Implemented_Object_Ptr ;
                        Initial_Offset : in Corba.Unsigned_Long)
                        return Corba.Unsigned_Long ;
   -- This function computes the size needed to marshall the object obj


   procedure Marshall (Obj : in Implemented_Object_Ptr ;
                       S : in out NetBufferedStream.Object'class) ;
   -- This procedure marshalls the object Obj into the stream S


   procedure Marshall (Obj : in Implemented_Object_Ptr ;
                       S : in out MemBufferedStream.Object'class) ;
   -- This procedure marshalls the object Obj into the stream S


   -----------------------------------------------
   ---     finalization operators              ---
   -----------------------------------------------

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
   --        SUBPROGRAMS DECLARATION            --
   -----------------------------------------------

   -----------------------------------------------
   --           marshalling operators           --
   -----------------------------------------------

   function Align_Size (Obj : in Object_Ptr ;
                        Initial_Offset : in Corba.Unsigned_Long)
                        return Corba.Unsigned_Long ;
   -- This function computes the size needed to marshall the object obj


   procedure Marshall (Obj : in Object_Ptr ;
                       S : in out NetBufferedStream.Object'Class) ;
   -- This procedure marshalls the object Obj into the stream S


   procedure Marshall (Obj : in Object_Ptr ;
                       S : in out MemBufferedStream.Object'Class) ;
   -- This procedure marshalls the object Obj into the stream S


   -----------------------------------------------
   --        constructors and destrucotrs       --
   -----------------------------------------------

   function Create_Omniobject(Most_Derived_Repoid : in Corba.String ;
                              Profiles : in Iop.Tagged_Profile_List ;
                              Release : in Corba.Boolean)
                              return Object_Ptr ;
   -- this function is called by corba.object.create_ref,
   -- which is called by corba.object.unmarshall
   -- We use this when we want to unmarshall an object out of
   -- a bufferedstream, and to create a Corba.object.ref'class.


   function Omniobject_Duplicate(Self : in Object_Ptr) return Object_Ptr ;
   -- creates a new Object referencing the same
   -- omniObject_C2Ada
   -- see Ada_OmniObject.hh


   procedure Omniobject_Destructor(Self : in Object_Ptr) ;
   -- C++ destructor of Ada_omniObject
   -- calls omni::objectRelease on the omniObject_C2Ada
   -- and destroys the Ada_omniObject


   -----------------------------------------------
   --      registering into the ORB             --
   -----------------------------------------------


   procedure Omniobject_Dispose(Self : in Object_Ptr) ;
   -- tells the BOA this local object does not accept
   -- connexions any longer
   -- only called by Dispose_Object(Implemented_Object)


   -----------------------------------------------
   --             object <-> IOR                --
   -----------------------------------------------

   function String_To_Object(RepoId : in Corba.String)
                             return Object_Ptr ;
   -- this function trys to create the Object
   -- corresponding to this resitory_id,
   -- returns null on failure


   function Object_To_String(Obj_ptr : in Object_Ptr) return Corba.String ;
   -- returns the IOR for this object
   -- Obj can be null

   -----------------------------------------------
   --          miscellaneous                    --
   -----------------------------------------------

   function Is_Proxy (Self : in Object'Class)
                      return Boolean ;
   -- returns true if this is a proxy object
   -- by calling the C++ function on the omniobject

   function Non_Existent(Self : in Object'Class) return Corba.Boolean ;
   -- returns true if the ORB is sure that this object
   -- does not exist

   function Hash(Self : in Object'Class ;
                 Maximum : in Corba.Unsigned_Long) return Corba.Unsigned_Long ;
   -- returns a hash value for this object

   procedure Assert_Object_Existent(Self : in Object'Class) ;
   pragma Import (CPP, Assert_Object_Existent, "assertObjectExistent__14Ada_OmniObject") ;
   -- checks that this object really exists

   procedure Get_Rope_And_Key (Self : in Object'Class ;
                               L : in out Omniropeandkey.Object ;
                               Success : out Boolean ) ;
   -- returns the rope and key for this omniobject
   -- if it is a proxy object


   procedure Set_Rope_And_Key(Self : in out Object'Class ;
                              L : in out Omniropeandkey.Object ;
                              KeepIOP : in Boolean := True) ;
   -- sets the rope and key for this object

   procedure Reset_Rope_And_Key(Self : in Object'Class) ;
   pragma Import(CPP, Reset_Rope_And_Key, "resetRopeAndKey__14Ada_OmniObject") ;
   -- re-sets the rope and key for this object


   function Get_Repository_Id(Self : in Object'class)
                              return Corba.String ;
   -- returns the repository_id for this object


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


  type Implemented_Object is abstract new Ada.Finalization.Limited_Controlled with record
      Omniobj : Object_Ptr ;
      Dispatch : Dispatch_Procedure ;
      Is_A : Is_A_Function ;
  end record ;


   -----------------------------------------------
   --             Omniobject                    --
   --     this type is imported from C++        --
   --   it is the equivalent of omniObject      --
   -----------------------------------------------
   --           PRIVATE PART                    --
   -----------------------------------------------

   package Address_To_Object is new System.Address_To_Access_Conversions (Object) ;
   function To_Object_Ptr is new Ada.Unchecked_Conversion(Address_To_Object.Object_Pointer,
                                                          Object_Ptr) ;
   function From_Object_Ptr is new Ada.Unchecked_Conversion(Object_Ptr,
                                                            Address_To_Object.Object_Pointer) ;
   -- useful routines to convert to/from C types.



   procedure Omniobject_Is_Ready(Self : in Object_Ptr) ;
   -- registers a local object into the ORB
   -- only called by Object_Is_Ready(Implemented_Object)

   procedure Set_Repository_Id(Self : in out Object'class ;
                               Repo_Id : in Corba.String) ;
   -- sets the reopsitory ID for this object
   -- it is called by Init(Omniobject.Implemented_Object)


   procedure C_Dispatch (Self : in Object'Class ;
                        Orls : in out Giop_S.Object ;
                        Orl_Op : in Interfaces.C.Strings.Chars_Ptr ;
                        Orl_Response_Expected : in Sys_Dep.C_Boolean ;
                        Success : out Sys_Dep.C_Boolean) ;
   pragma Export (CPP,C_Dispatch,
                  "dispatch__14Ada_OmniObjectR10Ada_Giop_sPCcbRb");
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



end OmniObject ;
