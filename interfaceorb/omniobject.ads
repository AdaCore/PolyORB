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

   type Object is abstract tagged record
      Table : Interfaces.CPP.Vtable_Ptr ;
   end record ;

   pragma CPP_Class (Object);
   pragma CPP_Vtable (Object,Table,1);
   -- This object is wrapped around Ada_OmniObject (see Ada_OmniObject.hh)

   type Object_Access is access Object ;
   -- just to give a name to pointers on Object


   procedure C_Init (Self : in out Object'Class ;
                     Manager : in System.Address) ;
   pragma Import (C,C_Init,"Init__14Ada_OmniObjectP17omniObjectManager") ;
   -- wrapper around Ada_OmniObject function Init
   -- (see Ada_OmniObject.hh)

   procedure Init (Self : in out Object'Class ;
                   Manager : in OmniObjectManager.Object) ;
   -- Ada equivalent of C procedure C_Init


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

   procedure Init (Self : in out Object'Class ;
                   RepoId : in String ;
                   R : in Rope.Object ;
                   Key : in Corba.Octet ;
                   Keysize : in Corba.Unsigned_Long ;
                   Profiles : in Iop.Tagged_Profile_List ;
                   Release : Corba.Boolean ) ;
   -- Ada equivalent of C procedure C_init2


   procedure C_PR_IRRepositoryId(Self : in Object'Class;
                                 RepositoryId : in Interfaces.C.Strings.Chars_Ptr ) ;
   pragma Import (C,C_PR_IRRepositoryId,
                  "PR_IRRepositoryId__14Ada_OmniObjectPCc") ;
   -- wrapper around  Ada_OmniObject function PR_IRRepositoryId
   -- (see Ada_OmniObject.hh)

   procedure PR_IRRepositoryId(Self : in Object'Class;
                               RepositoryId : in String ) ;
   -- Ada equivalent of C procedure C_PR_IRRepositoryId



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


   function C_Get_Rope_And_Key (Self : in Object'Class ;
                                L : in System.Address)
                                return Sys_Dep.C_Boolean ;
   pragma Import (CPP,C_Get_Rope_And_Key,
                  "getRopeAndKey__C10omniObjectR14omniRopeAndKey") ;
   -- wrapper around  Ada_OmniObject function getRopeAndKey
   -- (see Ada_OmniObject.hh)

   function Get_Rope_And_Key (Self : in Object'Class ;
                              L : in Omniropeandkey.Object)
                              return Boolean ;
   -- Ada equivalent of C function C_Get_Rope_And_Key


   procedure Assert_Object_Existent (Self : in Object'Class) ;
   pragma Import (CPP,Assert_Object_Existent,
                  "assertObjectExistent__10omniObject");
   -- wrapper around  Ada_OmniObject function assertObjectExistent
   -- (see Ada_OmniObject.hh)
   -- no Ada equivalent since there is no arguments


   function C_Is_Proxy (Self : in Object'Class)
                        return Sys_Dep.C_Boolean ;
   pragma Import (C,C_Is_Proxy,"is_proxy__14Ada_OmniObject") ;
   -- wrapper around Ada_OmniObject function is_proxy
   -- (see Ada_OmniObject.hh)

   function Is_Proxy (Self : in Object'Class)
                      return Boolean ;
   -- Ada equivalent of C function C_Is_Proxy


   function C_Dispatch (Self : in Object'Class ;
                        Orls : in System.Address ;
                        Orl_Op : in Interfaces.C.Strings.Chars_Ptr ;
                        Orl_Response_Expected : in Sys_Dep.C_Boolean)
                        return Sys_Dep.C_Boolean ;
   pragma Export (CPP,C_Dispatch,
                  "dispatch__10omniObjectR6GIOP_SPCcb");
   -- wrapper around Ada_OmniObject function dispatch
   -- (see Ada_OmniObject.hh)
   -- This function is implemented in Ada and exported to C
   -- it calls th Ada function Dispatch

   function Dispatch (Self : in Object'Class ;
                      Orls : in Giop_S.Object ;
                      Orl_Op : in String ;
                      Orl_Response_Expected : in Boolean)
                      return Boolean ;
   -- Ada equivalent of C function C_Dispatch
   -- this function is called by the C one
   -- It is not implemented here but in the sub-classes of omniObject

   procedure Object_Is_Ready(Self : in Object'Class) ;
   pragma Import (C,Object_Is_Ready,"objectIsReady__4omniP10omniObject") ;
   -- wrapper around omniORB's omni::objectIsReady in objectRef.CC L 230
   -- No Ada equivalent since there is no arguments


private

   function Constructor return Object'Class;
   pragma CPP_Constructor (Constructor);
   pragma Import (CPP,Constructor,"__14Ada_OmniObject");
   -- wrapped around the C constructor of Ada_OmniObject

end OmniObject ;








