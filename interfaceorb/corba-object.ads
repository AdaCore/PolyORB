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


   procedure Marshal_Obj_Ref(The_Object: Ref'Class ;
                           RepoID : String ;
                           Nbs : NetBufferedStream.Object) ;
   -- wrapper around void CORBA::MarshalObjRef(CORBA::Object_ptr obj,
   --        const char* repoId,
   --        size_t repoIdSize,
   --        NetBufferedStream &s)
   -- defined in objectRef.cc L721

   procedure Marshal_Obj_Ref(The_Object: Ref'Class ;
                           RepoID : String ;
                           Mbs : MemBufferedStream.Object) ;
   -- wrapper around void CORBA::MarshalObjRef(CORBA::Object_ptr obj,
   --        const char* repoId,
   --        size_t repoIdSize,
   --        MemBufferedStream &s)
   -- defined in objectRef.cc L850

   function UnMarshal_Obj_Ref(Repoid : in String ;
                            Nbs : in NetBufferedStream.Object
                           ) return Ref'Class ;
   -- return ???
   -- wrapper around CORBA::Object_ptr
   --                CORBA::UnMarshalObjRef(
   --                           const char* repoId,
   --                           NetBufferedStream& s)
   -- in objectRef.cc L637

   function UnMarshal_Obj_Ref(Repoid : in String ;
                            Mbs : in MemBufferedStream.Object
                           ) return Ref'Class ;
   -- return ???
   -- wrapper around CORBA::Object_ptr
   --                CORBA::UnMarshalObjRef(
   --                           const char* repoId,
   --                           MemBufferedStream& s)
   -- in objectRef.cc L765

   function Aligned_Obj_Ref(The_Object : Ref'Class ;
                          RepoID : String ;
                          Initial_Offset : Integer
                         ) return Integer ;
   -- wrapper around size_t
   --                CORBA::AlignedObjRef(CORBA::Object_ptr obj,
   --                const char* repoId,
   --                size_t repoIdSize,
   --                size_t initialoffset)
   -- in objectRef.cc L744

   -----------------------------------------------------
   ----      all the following methods are wrappers ----
   ----          around omniobject.adb              ----
   ----  to simulate C++ multiple inheritance       ----
   -----------------------------------------------------

   function Is_Proxy return Boolean ;

   procedure PR_IRRepositoryId(RepositoryId : in String ) ;

   procedure Init (Self : in out Object ;
                   Manager : in OmniObjectManager.Object);

   procedure Set_Rope_And_Key (Self : in out Object ;
                            L : in out Omniropeandkey.Object ;
                            KeepIOP : Corba.boolean
                           ) ;

   procedure Get_Rope_And_Key (Self : in Object ;
                           L : in out Omniropeandkey.Object ;
                           Result : out Corba.Boolean) ;

   procedure Assert_Object_Existent (Self : in Object) ;

   procedure Reset_Rope_And_Key (Self : in Object);


private

   type Dynamic_Type is access Ref'Class ;
   -- needed for compilation !! (Fabien)

   type Ref is new Ada.Finalization.Controlled with record
      Dynamic_Object : Dynamic_Type := null ;
      Omniobject : Omniobject.Object ;
   end record ;

   procedure Initialize (Self: in out Ref'Class);
   -- called each time a Ref object is created
   -- initializes the Dynamic_Object pointer
   -- creates the underlying C object

   procedure Adjust (Self: in out Ref'Class);
   -- called each time you duplicate a Ref object using :=
   -- update the value of the Dynamic_Object pointer

   procedure Finalize (Self: in out Ref'Class);
   -- called each time a Ref object must be trashed
   -- releases the underlying C pointer

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
