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

with Iop ;
with Omniobject ;

-- with Omniobjectmanager, Omniropeandkey ;

with NetBufferedStream, MemBufferedStream ;
with Giop_S ;

with Ada.Finalization ;

package Corba.Object is

   type Ref is tagged private ;
   type Ref_Ptr is access all Ref'Class ;
   type Constant_Ref_Ptr is  access constant Corba.Object.Ref'Class ;


   -- constant Ref
   Nil_Ref : aliased constant Ref ;

   --I boolean is_nil();
   function Is_Nil(Self: in Ref'Class) return Corba.Boolean;
   function Is_Null(Self: in Ref'Class) return Corba.Boolean renames Is_Nil;

   --I void release();
   procedure Release (Self : in out Ref'class);

   --I Object duplicate();
   -- use assignment

   function Is_A(Self: in Ref ;
                 Logical_Type_Id : in Corba.String)
                 return Corba.Boolean ;
  -- returns true if this object is of this Logical_Type_Id
   -- or one of its descendants

   function Is_A(Logical_Type_Id : in Corba.String)
                 return Corba.Boolean ;
   -- sam, but non dispatching, must be called
   -- Corba.Object.Is_A(...)

   function Non_Existent(Self : in Ref) return Corba.Boolean ;
   -- returns false if this reference is nil
   -- it could be smarter and call omniORB's non_existent
   -- in corbaObject.cc


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
   function Get_Dynamic_Type(Self: in Ref) return Ref'Class ;
   function Get_Nil_Ref(Self: in Ref) return Ref ;

   Repository_Id : Corba.String := Corba.To_Corba_String("IDL:omg.org/CORBA/Object:1.0") ;
   function Get_Repository_Id(Self : in Ref) return Corba.String ;

   function To_Ref(The_Ref: in Corba.Object.Ref'Class) return Ref ;

   procedure Assert_Ref_Not_Nil(Self : in Ref) ;


   function Object_To_String (Self : in CORBA.Object.Ref'class)
                              return CORBA.String ;
   -- returns the IOR corresponding to this object

   procedure String_to_Object (From : in CORBA.String;
                               To : out CORBA.Object.Ref'class) ;
   -- creates an object out of an IOR

   function Get_Profile_List (Obj : in Ref)
                              return Iop.Tagged_Profile_List ;
   -- return the Profile_List of an Object

   --------------------------------------------------
   ---        omniORB specific                    ---
   --------------------------------------------------

   procedure Create_Proxy_Object_Factory(RepoID : in Corba.String) ;
   -- stores in a global variable in the ORB the fact
   -- that a new IDL interface can be used
   -- must be called when elaborating each package
   -- where a descendant of Corba.Object.Ref is defined

   procedure Object_Is_Ready(Self : in Ref'Class) ;
   -- calls the C++ function omni::objectIsReady
   -- has to be done when an object has been created
   -- to register it into the ORB
   -- (as a proxy object)
   -- BEWARE : MUST BE CALLED ONLY ONCE FOR EACH OBJECT



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

   procedure Marshal_Object_Reference(The_Ref : in Ref ;
                                      S : in out NetBufferedStream.Object) ;

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
      Omniobj : Omniobject.Object_Ptr := null ;
      Dynamic_Type : Constant_Ref_Ptr := null ;
   end record ;

   procedure Initialize (Self: in out Ref);
   -- nothing to do

   procedure Adjust (Self: in out Ref);
   -- duplicate the underlying omniobject

   procedure Finalize (Self: in out Ref);
   -- release th underlying omniobject

   Nil_Ref :  aliased constant Ref := (Ada.Finalization.Controlled
                                       with Omniobj => null,
                                       Dynamic_Type => null) ;



end Corba.Object ;
