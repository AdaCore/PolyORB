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

-- with Omniobjectmanager, Omniropeandkey ;

with NetBufferedStream, MemBufferedStream ;
with Giop_S ;

with Ada.Finalization ;

package Corba.Object is

   -- proxy objects are references to implementations
   type Ref is tagged private ;
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
   --
   -- NONONONONONONONONONONONONO
   -- it is implemented !!! (objectRef.cc
   -- here, we can call finalize on the object,
   -- and that will call release on the C++ object

   --I Object duplicate();
   -- use assignment

   function Is_A(Self: in Ref ;
                 Logical_Type_Id : in Corba.Repository_Id)
                 return Corba.Boolean ;
  -- returns true if this object is of this Logical_Type_Id
   -- or one of its descendants

   function Non_Existent(Self : in Ref) return Corba.Boolean ;

   function Is_Equivalent(Self : in Ref ;
                          Other : in Ref)
                          return Corba.Boolean ;

   function Hash(Self : in Ref ;
                 Maximum : in Corba.Unsigned_long)
                 return Corba.Unsigned_Long ;

   -- get_implementation and get_interface
   -- are not supported

   --------------------------------------------------
   ---        AdaBroker  specific                 ---
   --------------------------------------------------
   function Get_Dynamic_Ref(Self: in Ref) return Ref'Class ;

   function To_Ref(The_Ref: in Corba.Object.Ref'Class) return Ref ;

   procedure AdaBroker_Cast_To_Parent(Real_Ref: in Ref;
                                      Result: out Corba.Object.Ref'Class ) ;

   procedure Assert_Ref_Not_Nil(Self : in Ref) ;


   --------------------------------------------------
   ---        omniORB specific                    ---
   --------------------------------------------------

   -- procedure AdaBroker_Dispatch (Self : in out Corba.Object.Object ;
   --                             Orls : in Giop_S.Object ;
   --                             Orl_Op : in Corba.String ;
   --                             Orl_Response_Expected : in Corba.Boolean ;
   --                             Returns : out Corba.Boolean ) ;
   -- dispatches a call received by the Orb to invoke
   -- the right function on a Corba.Object

   -- The five following subprograms are marshalling operators
   -- they will be inherited by all the descedants of Corba.Object.Ref
   -- so there is no need to redefine them in each new package
   -- ( it is done in C++ )
   --function NP_Aligned_Size(The_Ref : in Ref ;
   --                         Initial_Offset : in Corba.Unsigned_Long)
   --  return Corba.Unsigned_Long ;

   --procedure Marshal_Object_Reference(The_Ref : in Ref ;
   --                                   S : in out NetBufferedStream) ;

   --function Unmarshal_Object_Reference(S : in out NetBufferedStream)
   --  return Ref ;

   --procedure Marshal_Object_Reference(The_Ref : in Ref ;
   --                                   S : in out MemBufferedStream) ;

   --function Unmarshal_Object_Reference(S : in out MemBufferedStream)
   --  return Ref ;




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

   procedure Marshal_Obj_Ref(The_Ref: Ref'Class ;
                             RepoID : String ;
                             Nbs : NetBufferedStream.Object) ;
   -- wrapper around void CORBA::MarshalObjRef(CORBA::Object_ptr obj,
   --        const char* repoId,
   --        size_t repoIdSize,
   --        NetBufferedStream &s)
   -- defined in objectRef.cc L721

   procedure Marshal_Obj_Ref(The_Ref: Ref'Class ;
                             RepoID : String ;
                             Mbs : MemBufferedStream.Object) ;
   -- wrapper around void CORBA::MarshalObjRef(CORBA::Object_ptr obj,
   --        const char* repoId,
   --        size_t repoIdSize,
   --        MemBufferedStream &s)
   -- defined in objectRef.cc L850

   function UnMarshal_Obj_Ref(Repoid : in String ;
                              Nbs : in NetBufferedStream.Object'Class)
                              return Ref'Class ;
   -- return ???
   -- wrapper around CORBA::Object_ptr
   --                CORBA::UnMarshalObjRef(
   --                           const char* repoId,
   --                           NetBufferedStream& s)
   -- in objectRef.cc L637

   function UnMarshal_Obj_Ref(Repoid : in String ;
                              Mbs : in MemBufferedStream.Object'Class)
                              return Ref'Class ;
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


   function Aligned_Obj_Ref(The_Ref : Ref'Class ;
                            RepoID : String ;
                            Initial_Offset : Integer)
                            return Integer ;
   -- wrapper around size_t
   --                CORBA::AlignedObjRef(CORBA::Object_ptr obj,
   --                const char* repoId,
   --                size_t repoIdSize,
   --                size_t initialoffset)
   -- in objectRef.cc L744



private


   --------------------------------------------------
   ---     references to implementations          ---
   --------------------------------------------------
   type Ref is new Ada.Finalization.Controlled with record
      -- Dynamic_Ref : Ref_Ptr := null ;
      -- Omniobj : Omniobject.Object ;
      Is_Nil : Boolean := False ;
   end record ;

   procedure Initialize (Self: in out Ref);
   -- called each time a Ref object is created
   -- initializes the Dynamic_Object pointer
   -- creates the underlying C object

   procedure Adjust (Self: in out Ref);
   -- called each time you duplicate a Ref object using :=
   -- update the value of the Dynamic_Object pointer

   procedure Finalize (Self: in out Ref);
   -- called each time a Ref object must be trashed
   -- releases the underlying C pointer

   Nil_Ref : constant Ref := ( Ada.Finalization.Controlled with Is_Nil => True) ;

   --------------------------------------------------
   ---     real implementations of objects        ---
   --------------------------------------------------
   type Object is new Ada.Finalization.Controlled with null record ;


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













