-----------------------------------------------------------------------
----                                                               ----
----                  AdaBroker                                    ----
----                                                               ----
----                  package CORBA.Object                         ----
----                                                               ----
----   authors : Sebastien Ponce, Fabien Azavant                   ----
----   date    : 02/08/99                                          ----
----                                                               ----
----                                                               ----
-----------------------------------------------------------------------

with Omniobject, NetBufferedStream, MemBufferedStream ;
with Ada.Finalization ;

package Corba.Object is

   type Ref is tagged private;

   --I boolean is_nil();
   function Is_Nil(Self: in Ref'Class) return Boolean;
   function Is_Null(Self: in Ref'Class) return Boolean renames Is_Nil;

   --I void release();
   procedure Release (Self : in out Ref'class);
   -- Not implemented in omniORB
   -- neither here

   --I Object duplicate();
   -- use assignment


   --------------------------------------------------
   ---        omniORB specific                    ---
   --------------------------------------------------

   procedure PR_Setobj(The_Object : in OmniObject.Object) ;
   -- wrapper around void CORBA::Object::PR_setobj(omniObject *obj)
   -- in corbaObject.cc L121

   function PR_Getobj return OmniObject.Object ;
   -- wrapper around omniObject* CORBA::Object::PR_getobj()
   -- in corbaObject.cc L128

   --------------------------------------------------
   ---        AdaBroker  specific                 ---
   --------------------------------------------------
   function Get_Dynamic_Object(Self: in Ref'Class) return Ref'Class ;


   procedure MarshalObjRef(The_Object: Ref'Class ;
                           RepoID : String ;
                           Nbs : NetBufferedStream.Object) ;
   -- wrapper around void CORBA::MarshalObjRef(CORBA::Object_ptr obj,
   --        const char* repoId,
   --        size_t repoIdSize,
   --        NetBufferedStream &s)
   -- defined in objectRef.cc L721

   procedure MarshalObjRef(The_Object: Ref'Class ;
                           RepoID : String ;
                           Mbs : MemBufferedStream.Object) ;
   -- wrapper around void CORBA::MarshalObjRef(CORBA::Object_ptr obj,
   --        const char* repoId,
   --        size_t repoIdSize,
   --        MemBufferedStream &s)
   -- defined in objectRef.cc L850

   function UnMarshalObjRef(Repoid : in String ;
                            Nbs : in NetBufferedStream.Object
                           ) return Ref'Class ;
   -- return ???
   -- wrapper around CORBA::Object_ptr
   --                CORBA::UnMarshalObjRef(
   --                           const char* repoId,
   --                           NetBufferedStream& s)
   -- in objectRef.cc L637

   function UnMarshalObjRef(Repoid : in String ;
                            Mbs : in MemBufferedStream.Object
                           ) return Ref'Class ;
   -- return ???
   -- wrapper around CORBA::Object_ptr
   --                CORBA::UnMarshalObjRef(
   --                           const char* repoId,
   --                           MemBufferedStream& s)
   -- in objectRef.cc L765

   function AlignedObjRef(The_Object : Ref'Class ;
                          RepoID : String ;
                          Initial_Offset : Integer
                         ) return Integer ;
   -- wrapper around size_t
   --                CORBA::AlignedObjRef(CORBA::Object_ptr obj,
   --                const char* repoId,
   --                size_t repoIdSize,
   --                size_t initialoffset)
   -- in objectRef.cc L744


private

   type Ref is new Ada.Finalization.Controlled with record
      Dynamic_Object : access Ref'Class := null ;
   end record ;

   procedure Initialise (Self: in out Ref'Class);
   -- called each time a Ref object is created
   -- initialise the Dynamic_Object pointer

   procedure Adjust (Self: in out Ref'Class);
   -- called each time you duplicate a Ref object using :=
   -- update the value of the Dynamic_Object pointer

   procedure Finalize (Self: in out Ref'Class);
   -- called each time a Ref object must be trashed
   -- nothing to do for the moment

   -------------------------------------------------------------------
   -- ancienne methode compliquee sans les types controlled
   --
   --   type Internal_Object ;
   --   type Internal_Object_Access is access all Internal_Object'Class ;
   --
   --   type Dynamic_Type(Ptr : access Internal_Object'Class) is limited null record ;
   --
   --   type Internal_Object is tagged limited record
   --      Dynamic_Object : Dynamic_Type(Internal_Object'Access) ;
   --   end record ;
   --
   --   type Ref is tagged record
   --      Internal_Corba_Object : Internal_Object_Access ;
   --   end record ;

   end Corba.Object ;
