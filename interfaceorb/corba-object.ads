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

-- with Omniobject ;
-- removed because circular dependency

with Omniobjectmanager, Omniropeandkey ;

with NetBufferedStream, MemBufferedStream ;
with Ada.Finalization ;

package Corba.Object is

   type Ref is tagged private ;

   type Ref_Access is access Ref ;

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

   -- procedure PR_Setobj(The_Object : in OmniObject.Object) ;
   -- wrapper around void CORBA::Object::PR_setobj(omniObject *obj)
   -- in corbaObject.cc L121
   --
   -- should not be useful any longer

   -- function PR_Getobj return OmniObject.Object ;
   -- wrapper around omniObject* CORBA::Object::PR_getobj()
   -- in corbaObject.cc L128
   --
   -- should not be useful any longer

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
                            Nbs : in NetBufferedStream.Object'Class
                           ) return Ref'Class ;
   -- return ???
   -- wrapper around CORBA::Object_ptr
   --                CORBA::UnMarshalObjRef(
   --                           const char* repoId,
   --                           NetBufferedStream& s)
   -- in objectRef.cc L637

   function UnMarshal_Obj_Ref(Repoid : in String ;
                            Mbs : in MemBufferedStream.Object'Class
                           ) return Ref'Class ;
   -- return ???
   -- wrapper around CORBA::Object_ptr
   --                CORBA::UnMarshalObjRef(
   --                           const char* repoId,
   --                           MemBufferedStream& s)
   -- in objectRef.cc L765

   function UnMarshal_Obj_Ref(Nbs : in NetBufferedStream.Object'Class)
                              return Ref'Class ;
   function UnMarshal_Obj_Ref(Nbs : in MemBufferedStream.Object'Class)
                              return Ref'Class ;
   -- wrappers around
   -- static Object_ptr unmarshalObjRef(NetBufferedStream& s);
   -- static Object_ptr unmarshalObjRef(MemBufferedStream& s);
   -- CORBA.H, L1344


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

   procedure Init (Self : in out Corba.Object.Ref'Class ;
                   Manager : in OmniObjectManager.Object);

   procedure Set_Rope_And_Key (Self : in out Corba.Object.Ref'Class ;
                            L : in out Omniropeandkey.Object ;
                            KeepIOP : Corba.Boolean := True
                           ) ;

   procedure Get_Rope_And_Key (Self : in Corba.Object.Ref'Class ;
                               L : in out Omniropeandkey.Object ;
                               Result : out Corba.Boolean) ;

   procedure Assert_Object_Existent (Self : in Corba.Object.Ref'Class) ;

   procedure Reset_Rope_And_Key (Self : in Corba.Object.Ref'Class);


private

   type Dynamic_Type is access Ref'Class ;
   -- needed for compilation !! (Fabien)

   type Ref is new Ada.Finalization.Controlled with record
      Dynamic_Object : Dynamic_Type := null ;
      -- Wrapped_Omniobject : Omniobject.Object ;
      -- removed because circular dependency
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

end Corba.Object ;
