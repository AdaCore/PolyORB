--  This package is wrapped around a C++ class whose name is
--  Ada_OmniObject.  It provides two types of methods : the C functions of
--  the Ada_OmniObject class and their equivalent in Ada. (he first ones
--  have a C_ prefix.)  In addition, there is a raise_ada_exception
--  function that allows C functions to raise the ada No_Initialisation
--  exception.

with Ada.Unchecked_Conversion;
with Ada.Finalization;

with Interfaces.CPP;
with Interfaces.C.Strings;

with System;
with System.Address_To_Access_Conversions;

with CORBA;

with GIOP_S;
with Sys_Dep;
with IOP;
with OmniRopeAndKey;
with NetBufferedStream;
with MemBufferedStream;

package OmniObject is

   --  Implemented_Object this is the type of local implementations of
   --  objects it is the root of all XXX.Impl.Object


   type Implemented_Object is abstract tagged limited private;

   type Implemented_Object_Ptr is access all Implemented_Object'Class;

   type Dispatch_Procedure is access
     procedure (Self                  : Implemented_Object_Ptr;
                Orls                  : in out GIOP_S.Object;
                Orl_Op                : in Standard.String;
                Orl_Response_Expected : in CORBA.Boolean;
                Success               : out CORBA.Boolean);
   --  This type is used to make the dispatchnig call.  this type is made
   --  to handle dispatchnig calls from the ORB. This procedure cannot be
   --  made a primitive of Implemented_Object, therwise it would have to
   --  for each descendant in the user's code.

   type Is_A_Function is access
     function (Repoid : in CORBA.String) return CORBA.Boolean;
   --  This type is used to make the dispatchnig call.  this type is made
   --  to handle dispatchnig calls from the ORB. This procedure cannot be
   --  made a primitive of Implemented_Object, therwise it would have to
   --  for each descendant in the user's code.


   --  Omniobject this type is imported from C++ it is the equivalent of
   --  omniObject

   type Object is tagged limited record
      Implobj  : Implemented_Object_Ptr := null;
      C_Object : System.Address;
      Init_Ok  : Sys_Dep.Bool;
      Table    : Interfaces.CPP.Vtable_Ptr;
   end record;
   --  Implobj  (Ada) : the pointer implobj is null for proxy object
   --  C_Object (C)   : pointer on the underlying omniObject_C2Ada object
   --  Init_Ok  (C)   : state of the object (initialized or not)
   --  Table    (C)   : needed to interface C++ and Ada

   pragma Cpp_Class (Object);
   pragma Cpp_Vtable (Object, Table, 2);
   --  This type is both a C and an Ada class it is is wrapped around
   --  Ada_OmniObject (see Ada_OmniObject.hh)

   type Object_Ptr is access all Object'Class;


   --  Implemented_Object this is the type of local implementations of
   --  objects it is the root of all XXX.Impl.Object

   procedure Init_Local_Object
     (Self   : in out Implemented_Object;
      Repoid : in CORBA.String;
      Disp   : in Dispatch_Procedure;
      Isa    : in Is_A_Function);
   --  Calls the C++ Init to set the init_ok boolean to true and sets the
   --  repoID of this object. It also sets the dispatch procedure
   --  associated with this object

   function Is_Nil  (Self : in Implemented_Object) return CORBA.Boolean;
   function Is_Null (Self : in Implemented_Object) return CORBA.Boolean
     renames Is_Nil;
   --  Returns True if this is a nil object


   function Get_Repository_Id
     (Self : in Implemented_Object)
      return CORBA.String;
   --  Return the repository_id of the C++ object


   Repository_Id : CORBA.String
     := CORBA.To_CORBA_String
     (Standard.String'("IDL:omg.org/CORBA/Object:1.0"));

   function Hash
     (Self    : in Implemented_Object;
      Maximum : in CORBA.Unsigned_Long)
      return CORBA.Unsigned_Long;
   --  Returns a hash value for this object

   function Get_Object_Ptr
     (Self : in Implemented_Object)
      return Object_Ptr;
   --  Returns its underlying Object


   -------------------------
   -- Registering the ORB --
   -------------------------

   procedure Object_Is_Ready (Self : in Implemented_Object'Class);
   --  Tells the ORB that this object is ready to accept connections it has
   --  to be done once (and only once) for each local object this function
   --  *must not* be called directly by an implmentation.  Call the
   --  Boa.Object_Is_Ready instead. There is no difference in this version
   --  of AdaBroker since there is only one BOA, but there may be in the
   --  future.


   procedure Dispose_Object (Self : in Implemented_Object'Class);
   --  Releases all the ressources associated to a local object it is done
   --  automatically at the end of the scope, but can be done prematuraly
   --  by the invokation of this procedure.  Once this procedure has been
   --  called, the Implemented_Object becomes a null object.


   --------------------
   -- object <-> IOR --
   --------------------

   function Object_To_String
     (Self : in Implemented_Object'Class)
      return CORBA.String;
   --  Return the IOR corresponding to this object



   ---------------------------
   -- Marshalling operators --
   ---------------------------

   function Align_Size
     (Obj            : in Implemented_Object_Ptr;
      Initial_Offset : in CORBA.Unsigned_Long)
      return CORBA.Unsigned_Long;
   --  This function computes the size needed to marshall the object obj

   procedure Marshall
     (Obj : in Implemented_Object_Ptr;
      S   : in out NetBufferedStream.Object'class);
   --  This procedure marshalls the object Obj into the stream S

   procedure Marshall
     (Obj : in Implemented_Object_Ptr;
      S : in out MemBufferedStream.Object'class);
   --  This procedure marshalls the object Obj into the stream S


   ----------------------------
   -- finalization operators --
   ----------------------------

   procedure Initialize (Self : in out Implemented_Object);
   --  Create the underlying Omniobject both objects point on one another

   procedure Adjust (Self : in out Implemented_Object);
   --  Create the underlying Omniobject both objects point on one another
   --  sets the repository ID

   procedure Finalize (Self : in out Implemented_Object);
   --  Release the underlying omniobject


   --  Omniobject this type is imported from C++ it is the equivalent of
   --  omniObject

   ---------------------------
   -- Marshalling operators --
   ---------------------------

   function Align_Size
     (Obj            : in Object_Ptr;
      Initial_Offset : in CORBA.Unsigned_Long)
      return CORBA.Unsigned_Long;
   --  This function computes the size needed to marshall the object obj

   procedure Marshall
     (Obj : in Object_Ptr;
      S : in out NetBufferedStream.Object'Class);
   --  This procedure marshalls the object Obj into the stream S

   procedure Marshall
     (Obj : in Object_Ptr;
      S   : in out MemBufferedStream.Object'Class);
   --  This procedure marshalls the object Obj into the stream S


   ----------------------------------
   -- constructors and destructors --
   ----------------------------------

   function Create_OmniObject
     (Most_Derived_Repoid : in CORBA.String;
      Profiles            : in IOP.Tagged_Profile_List;
      Release             : in CORBA.Boolean)
      return Object_Ptr;
   --  This function is called by corba.object.create_ref, which is called
   --  by corba.object.unmarshall We use this when we want to unmarshall an
   --  object out of a bufferedstream, and to create a
   --  CORBA.object.ref'class.

   function OmniObject_Duplicate (Self : in Object_Ptr) return Object_Ptr;
   --  Creates a new Object referencing the same omniObject_C2Ada see
   --  Ada_OmniObject.hh

   procedure OmniObject_Destructor (Self : in Object_Ptr);
   --  C++ destructor of Ada_omniObject calls omni::objectRelease on the
   --  omniObject_C2Ada and destroys the Ada_omniObject


   ------------------------------
   -- registering into the ORB --
   ------------------------------

   procedure OmniObject_Dispose (Self : in Object_Ptr);
   --  Tells the BOA this local object does not accept connexions any
   --  longer only called by Dispose_Object(Implemented_Object)


   --------------------
   -- object <-> IOR --
   --------------------

   function String_To_Object
     (Repoid : in CORBA.String)
      return Object_Ptr;
   --  This function trys to create the Object corresponding to this
   --  resitory_id, returns null on failure

   function Object_To_String (Obj_Ptr : in Object_Ptr) return CORBA.String;
   --  Returns the IOR for this object Obj can be null

   function Is_Proxy
     (Self : in Object'Class)
      return Boolean;
   --  Returns true if this is a proxy object by calling the C++ function
   --  on the omniobject

   function Non_Existent
     (Self : in Object'Class)
      return CORBA.Boolean;
   --  Returns true if the ORB is sure that this object does not exist

   function Hash
     (Self    : in Object'Class;
      Maximum : in CORBA.Unsigned_Long)
      return CORBA.Unsigned_Long;
   --  Returns a hash value for this object

   procedure Assert_Object_Existent (Self : in Object'Class);
   pragma Import
     (CPP, Assert_Object_Existent, "assertObjectExistent__14Ada_OmniObject");
   --  Checks that this object really exists

   procedure Get_Rope_And_Key
     (Self    : in Object'Class;
      L       : in out OmniRopeAndKey.Object;
      Success : out Boolean);
   --  Returns the rope and key for this omniobject if it is a proxy object

   procedure Set_Rope_And_Key
     (Self    : in out Object'Class;
      L       : in out OmniRopeAndKey.Object;
      KeepIOP : in Boolean := True);
   --  Sets the rope and key for this object

   procedure Reset_Rope_And_Key (Self : in Object'Class);
   pragma Import
     (CPP, Reset_Rope_And_Key, "resetRopeAndKey__14Ada_OmniObject");
   --  Re-sets the rope and key for this object

   function Get_Repository_Id
     (Self : in Object'class)
      return CORBA.String;
   --  Returns the repository_id for this object

   function Get_Profile_List
     (Self : in Object'Class)
      return IOP.Tagged_Profile_List;
   --  Returns the Profile list of an object

private

   --  Implemented_Object this is the type of local implementations of
   --  objects it is the root of all XXX.Impl.Object

   type Implemented_Object is
     abstract new Ada.Finalization.Limited_Controlled with record
        OmniObj  : Object_Ptr;
        Dispatch : Dispatch_Procedure;
        Is_A     : Is_A_Function;
     end record;

   --  Omniobject this type is imported from C++ it is the equivalent of
   --  omniObject

   package Address_To_Object is
     new System.Address_To_Access_Conversions (Object);

   function To_Object_Ptr is
     new Ada.Unchecked_Conversion
     (Address_To_Object.Object_Pointer, Object_Ptr);

   function From_Object_Ptr is
     new Ada.Unchecked_Conversion
     (Object_Ptr, Address_To_Object.Object_Pointer);

   procedure OmniObject_Is_Ready (Self : in Object_Ptr);
   --  Registers a local object into the ORB only called by
   --  Object_Is_Ready(Implemented_Object)

   procedure Set_Repository_Id
     (Self   : in out Object'class;
      Repoid : in CORBA.String);
   --  Sets the reopsitory ID for this object it is called by
   --  Init(Omniobject.Implemented_Object)


   procedure C_Dispatch
     (Self                  : in Object'Class;
      Orls                  : in out GIOP_S.Object;
      Orl_Op                : in Interfaces.C.Strings.chars_ptr;
      Orl_Response_Expected : in Sys_Dep.Bool;
      Success               : out Sys_Dep.Bool);

   pragma Export
     (CPP, C_Dispatch, "dispatch__14Ada_OmniObjectR10Ada_Giop_sPCcbRb");
   --  Wrapper around Ada_OmniObject function dispatch (see
   --  Ada_OmniObject.hh) This function is implemented in Ada and exported
   --  to C it calls th Ada function Dispatch

   function  C_Is_A
     (Self   : in Object'Class;
      Repoid : in Interfaces.C.Strings.chars_ptr)
      return  Sys_Dep.Bool;

   pragma Export (CPP, C_Is_A, "Ada_Is_A__14Ada_OmniObjectPCc");
   --  Returns true if self is in repoID, or one of its descendant

   ---------------------
   -- memory handling --
   ---------------------

   function Constructor return Object'Class;
   pragma Import (CPP, Constructor, "__14Ada_OmniObject");
   pragma Cpp_Constructor (Constructor);
   --  Wrapped around the C constructor of Ada_OmniObject

   function Object_Ptr_Constructor return Object_Ptr;
   --  This is a workaround for gnat 3.11p we cannot write toto :
   --  Object_Ptr := new Object we have to call the C++ constructor to
   --  create objects

end OmniObject;
