-----------------------------------------------------------------------
----                                                               ----
----                       AdaBroker                               ----
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
----                  package omniObject                           ----
----                                                               ----
----   authors : Sebastien Ponce, Fabien Azavant                   ----
----   date    : 02/17/99                                          ----
----                                                               ----
----                                                               ----
-----------------------------------------------------------------------

with Ada.Finalization ;
with Interfaces.CPP ;
with Interfaces.C.Strings ;
with System ;
with Giop_S ;
with Corba ;
with Sys_Dep ;
with OmniObjectManager ;
with Iop ;
with Rope ;
with OmniRopeAndKey ;


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
      Cptr : System.Address ;
      -- this field is only used by the C++ side of the object
      Table : Interfaces.CPP.Vtable_Ptr ;
   end record ;
   -- the pointer implobj is null for proxy object

   pragma CPP_Class (Object);
   pragma CPP_Vtable (Object,Table,2);
   -- This object is wrapped around Ada_OmniObject (see Ada_OmniObject.hh)

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


   procedure Object_Is_Ready(Self : in Implemented_Object'Class) ;
   -- calls the C++ function omni::objectIsReady
   -- has to be done when an object has been created
   -- to register it into the ORB
   -- (as a local object)
   -- BEWARE : MUST BE CALLED ONLY ONCE FOR EACH OBJECT

   function Is_A(Self: in Implemented_Object ;
                 Logical_Type_Id : in Corba.String)
                 return Corba.Boolean ;
   -- returns true if this object is of this Logical_Type_Id
   -- or one of its descendants

   Repository_Id : Corba.String
     := Corba.To_Corba_String("IDL:omg.org/CORBA/Object:1.0") ;

   function Get_Repository_Id(Self : in Implemented_Object)
                              return Corba.String ;

   -----------------------------------------------
   --             Omniobject                    --
   --     this type is imported from C++        --
   --   it is the equivalent of omniObject      --
   -----------------------------------------------
   --        SUBPROGRAMS DECLARATION            --
   -----------------------------------------------

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



   procedure Set_Repository_Id(Self : in out Object'class ;
                               Repo_Id : in Corba.String) ;

   function Get_Repository_Id(Self : in Object'class)
                              return Corba.String ;





   procedure Init (Self : in out Object'Class ;
                   RepoId : in String ;
                   R : in Rope.Object ;
                   Key : in Corba.Octet ;
                   Keysize : in Corba.Unsigned_Long ;
                   Profiles : in Iop.Tagged_Profile_List ;
                   Release : Corba.Boolean ) ;
   -- initialization of proxy objects




   procedure C_Set_Rope_And_Key (Self : in out Object'Class ;
                                 L : in System.Address ;
                                 KeepIOP : in Sys_Dep.C_Boolean) ;
   pragma Import (C,C_Set_Rope_And_Key,
                  "setRopeAndKey__10omniObjectRC14omniRopeAndKeyb") ;
   -- wrapper around  Ada_OmniObject function setRopeAndKey
   -- (see Ada_OmniObject.hh)

   procedure Set_Rope_And_Key (Self : in out Object'Class ;
                               L : in Omniropeandkey.Object ;
                               KeepIOP : in Boolean) ;
   -- Ada equivalent of C procedure C_Set_Rope_And_Key



   procedure Assert_Object_Existent (Self : in Object'Class) ;
   pragma Import (CPP,Assert_Object_Existent,
                  "assertObjectExistent__10omniObject");
   -- wrapper around  Ada_OmniObject function assertObjectExistent
   -- (see Ada_OmniObject.hh)
   -- no Ada equivalent since there is no arguments


   function Is_Proxy (Self : in Object'Class)
                      return Boolean ;
   -- returns true if this is a proxy object
   -- there is no need to call the C++ function
   -- To Know It Because We Have The Information in Ada


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


   function Dispatch (Self : in Object'Class ;
                      Orls : in Giop_S.Object ;
                      Orl_Op : in String ;
                      Orl_Response_Expected : in Boolean)
                      return Boolean ;
   -- Ada equivalent of C function C_Dispatch
   -- this function is called by the C one
   -- It is not implemented here but in the sub-classes of omniObject



   function Constructor return Object'Class;
   pragma Import (CPP,Constructor,"__14Ada_OmniObject");
   pragma CPP_Constructor (Constructor);
   -- wrapped around the C constructor of Ada_OmniObject

   -- This is a workaround for gnat 3.11p
   -- we cannot write
   -- toto : Object_Ptr := new Object
   -- we have to call the C++ constructor to create objects
   function C_Object_Ptr_Constructor return System.Address ;
   pragma Import (CPP,C_Object_Ptr_Constructor,"__14Ada_OmniObject") ;

   function Object_Ptr_Constructor return Object_Ptr ;
   -- corresponding Ada function ;

   procedure Object_Destructor(Self : in out Object'Class) ;
   pragma Import (CPP,Object_Destructor,
                  "Destructor__14Ada_OmniObjectP14Ada_OmniObject") ;
   -- C++ destructor of Ada_omniObject


end OmniObject ;










