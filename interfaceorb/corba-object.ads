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

with Omniobject ;

with Omniobjectmanager, Omniropeandkey ;

with NetBufferedStream, MemBufferedStream ;
with Ada.Finalization ;

package Corba.Object is

   -- proxy objects are references to implementations
   type Ref is new  Omniobject.Object with private ;
   type Ref_Ptr is access all Ref ;
   Nil_Ref : constant Ref ;

   -- objects are real implementations of the object
   type Object is tagged private ;
   type Object_Ptr is access all Object ;

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

   function To_Ref(The_Ref: in Corba.Object.Ref'Class) return Ref ;

   procedure AdaBroker_Cast_To_Parent(Real_Object: in Ref;
                                      Result: out Corba.Object.Ref'Class ) ;

   procedure Assert_Ref_Not_Nil(Self : in Ref) ;


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


private


   type Ref is new Omniobject.Object with record
      Dynamic_Object : Ref_Ptr := null ;
      Is_Nil : Boolean := False ;
   end record ;

   Nil_Ref : constant Ref := (Omniobject.Object with Dynamic_Object => null,
                              Is_Nil => True ) ;


   type Object is new Omniobject.object with null record ;


   procedure Initialize (Self: in out Object);
   -- called each time a Ref object is created
   -- initializes the Dynamic_Object pointer
   -- creates the underlying C object

   procedure Adjust (Self: in out Object);
   -- called each time you duplicate a Ref object using :=
   -- update the value of the Dynamic_Object pointer

   procedure Finalize (Self: in out Object);
   -- called each time a Ref object must be trashed
   -- releases the underlying C pointer

end Corba.Object ;













