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
with System.Address_To_Access_Conversions ;

with Corba ;
with Sys_Dep ;
with OmniObjectManager ;
with Iop ;
with Rope ;
with OmniRopeAndKey ;


package OmniObject is

   type Object is new Ada.Finalization.Controlled with record
      Corba_Object_Ptr : System.Address ;
      Table : Interfaces.CPP.Vtable_Ptr ;
   end record ;

--   pragma CPP_Class (Object);
--   pragma CPP_Vtable (Object,Table,1);
   -- This object is wrapped around Ada_OmniObject (see Ada_OmniObject.hh)

   type Object_Access is access Object ;
   -- just to give a name to pointers on Object


   function C_Is_Proxy (Self : in Object'Class)
                        return Sys_Dep.C_Boolean ;
   pragma Import (C,C_Is_Proxy,"is_proxy__14Ada_OmniObject") ;
   -- wrapper around Ada_OmniObject function is_proxy
   -- (see Ada_OmniObject.hh)

   function Is_Proxy (Self : in Object'Class)
                      return Boolean ;
   -- Ada equivalent of C function C_Is_Proxy



   procedure C_PR_IRRepositoryId(Self : in Object'Class;
                                 RepositoryId : in Interfaces.C.Strings.Chars_Ptr ) ;
   pragma Import (C,C_PR_IRRepositoryId,"PR_IRRepositoryId__14Ada_OmniObjectPCc") ;
   -- wrapper around  Ada_OmniObject function PR_IRRepositoryId
   -- (see Ada_OmniObject.hh)

   procedure PR_IRRepositoryId(Self : in Object'Class;
                               RepositoryId : in String ) ;
   -- Ada equivalent of C procedure C_PR_IRRepositoryId



   procedure C_Init (Self : in out Object'Class ;
                     Manager : in System.Address) ;
   pragma Import (C,C_Init,"Init__14Ada_OmniObjectP17omniObjectManager") ;
   -- wrapper around Ada_OmniObject function Init
   -- (see Ada_OmniObject.hh)

   procedure Init (Self : in out Object'Class ;
                   Manager : in OmniObjectManager.Object) ;
   -- Ada equivalent of C procedure C_Init


   procedure C_Init (Self : in out Object'Class ;
                     RepoId : in Interfaces.C.Strings.Chars_Ptr ;
                     R : in System.Address ;
                     Key : in System.Address ;
                     Keysize : in Interfaces.C.Unsigned_Long ;
                     Profiles : in System.Address ;
                     Release : Sys_Dep.C_Boolean) ;
   pragma Import (C,C_Init,"Init__14Ada_OmniObjectPCcP4RopePUcUiPt25_CORBA_Unbounded_Sequence1ZQ23IOP13TaggedProfileb") ;
   -- wrapper around Ada_OmniObject function Init
   -- (see Ada_OmniObject.hh)

   procedure Init (Self : in out Object'Class ;
                   RepoId : in String ;
                   R : in Rope.Object ;
                   Key : in Corba.Octet ;
                   Keysize : in Corba.Unsigned_Long ;
                   Profiles : in Iop.Tagged_Profile_List ;
                   Release : Corba.Boolean ) ;
   -- Ada equivalent of C procedure C_init


   procedure C_Set_Rope_And_Key (Self : in out Object'Class ;
                               L : in out System.Address ;
                               KeepIOP : in Sys_Dep.C_Boolean) ;
   pragma Import (C,C_Set_Rope_And_Key,
                  "setRopeAndKey__10omniObjectRC14omniRopeAndKeyb") ;
   -- wrapper around void setRopeAndKey(const omniRopeAndKey& l,
   --                                   _CORBA_Boolean keepIOP=1);
   -- in omniInternal.h L 328

   procedure Set_Rope_And_Key (Self : in out Object'Class ;
                               L : in Omniropeandkey.Object ;
                               KeepIOP : in Corba.Boolean := True) ;
   -- Ada equivalent of C procedure C_Set_Rope_And_Key



   function C_Get_Rope_And_Key (Self : in Object'Class ;
                                L : in System.Address)
                                return Sys_Dep.C_Boolean ;
   pragma Import (CPP,C_Get_Rope_And_Key,
                  "getRopeAndKey__C10omniObjectR14omniRopeAndKey") ;
   -- wrapper around _CORBA_Boolean getRopeAndKey(omniRopeAndKey& l) const;
   -- in omniInternal.h L 338

   function Get_Rope_And_Key (Self : in Object'Class ;
                              L : in Omniropeandkey.Object)
                              return Corba.Boolean ;
   -- Ada equivalent of C function C_Get_Rope_And_Key



   procedure Assert_Object_Existent (Self : in Object'Class) ;
   pragma Import (CPP,Assert_Object_Existent,
                  "assertObjectExistent__10omniObject");
   -- wrapper around   void assertObjectExistent();
   -- in omniInternal.h L 356
   -- no "equivalent in Ada" since there is no arguments



   procedure Reset_Rope_And_Key (Self : in Object'Class);
   pragma Import (CPP,Reset_Rope_And_Key,
                  "resetRopeAndKey__10omniObject");
   -- wrapper around void resetRopeAndKey();
   -- in omniInternal.h L 332
   -- no "equivalent in Ada" since there is no arguments



   function C_Dispatch (Self : in Object'Class ;
                        Orls : in System.Address ;
                        Orl_Op : in Interfaces.C.Strings.Chars_Ptr ;
                        Orl_Response_Expected : in Sys_Dep.C_Boolean)
                        return Interfaces.C.Unsigned_Char ;
   -- implements virtual dispatch (GIOP_S &_ORL_s, const char *_ORL_op,
   --                              CORBA::Boolean _ORL_response_expected)
   -- in omniInternal.h L377
   pragma Export (CPP,C_Dispatch,
                  "dispatch__10omniObjectR6GIOP_SPCcb");


   procedure Object_Is_Ready(Self : in Object'Class) ;
   -- calls omniORB's omni::objectIsReady in objectRef.CC L 230


private

   procedure Initialize (Self: in out Object);
   -- called each time a Ref object is created

   procedure Adjust (Self: in out Object);
   -- called each time you duplicate a Ref object using :=

   procedure Finalize (Self: in out Object);
   -- called each time a Ref object must be trashed


end OmniObject ;








