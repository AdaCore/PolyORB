with CORBA;
with CORBA.Object;
with PortableServer; use PortableServer;
with PortableServer.AdapterActivator;
with PortableServer.ServantManager;
with Broca.Refs;
with Broca.Buffers;
with Broca.Locks;
with Broca.Sequences;

package Broca.POA is
   type POA_Object;
   type POA_Object_Ptr is access all POA_Object'Class;

   ------------------
   --  POAManager  --
   ------------------

   --  9.3.2  Processing States.
   --  A POA manager has four possible processing states: active,
   --  inactive, holding and discarding.
   type Processing_State_Type is (Holding, Active, Discarding, Inactive);

   --  POAManager internal object and primitives.
   type POAManager_Object is abstract new Broca.Refs.Ref_Type
     with null record;
   type POAManager_Object_Ptr is access all POAManager_Object'Class;

   procedure Activate (Self : in out POAManager_Object) is abstract;
   procedure Hold_Requests (Self : in out POAManager_Object;
                            Wait_For_Completion : CORBA.Boolean) is abstract;
   procedure Discard_Requests (Self : in out POAManager_Object;
                               Wait_For_Completion : CORBA.Boolean)
      is abstract;
   procedure Deactivate
     (Self : in out POAManager_Object;
      Etherealize_Objects : in CORBA.Boolean;
      Wait_For_Completion : in CORBA.Boolean) is abstract;

   --  Add (register) or remove (unregister) a POA to be controled by the
   --  POAManager.
   procedure Register (Self : in out POAManager_Object;
                       A_Poa : POA_Object_Ptr) is abstract;
   procedure Unregister (Self : in out POAManager_Object;
                         A_Poa : POA_Object_Ptr) is abstract;

   --  9.3.2
   --  A POA Manager encapsulates the processing state of the POAs it is
   --  associed with.
   --
   --  There is also a usage counter associed with the state.  It is used
   --  only to implement wait_for_completion.  However, it is externalized
   --  through these subprograms.
   --
   --  Get state and increment usage counter is the state is active.
   procedure Inc_Usage_If_Active (Self : in out POAManager_Object;
                                  State : out Processing_State_Type)
      is abstract;

   --  Increment usage counter.
   --  Very restricted usage:  only for wait completion during a deactivate
   --  call.
   procedure Inc_Usage (Self : in out POAManager_Object) is abstract;

   --  Decrement usage counter.
   procedure Dec_Usage (Self : in out POAManager_Object) is abstract;

   --  Return true is the POAManager is inactive.
   function Is_Inactive (Self : in POAManager_Object) return Boolean
      is abstract;

   -------------------------
   --  Internal_Skeleton  --
   -------------------------

   --  An internal seleton is the internal object for an object implementation.

   type Internal_Skeleton is new Broca.Refs.Ref_Type with
      record
         P_Servant : PortableServer.Servant;
      end record;

   type Internal_Skeleton_Ptr is access all Internal_Skeleton;

   --  Can raise bad_param.
   function To_Internal_Skeleton (Ref : CORBA.Object.Ref'Class)
                                  return Internal_Skeleton_Ptr;

   --  Also set usage counter to 1.
   function Create_Internal_Skeleton (P_Servant : PortableServer.Servant)
                                      return Internal_Skeleton_Ptr;

   ----------------
   --  Skeleton  --
   ----------------

   --  A Skeleton is the internal object for an object implementation.

   type Skeleton is new Broca.Refs.Ref_Type with
      record
         --  IOR created for this object.
         IOR : Broca.Buffers.Buffer_Descriptor;

         P_Servant : PortableServer.Servant;

         POA : Broca.POA.POA_Object_Ptr;

         --  ObjectId.
         Object_Id : PortableServer.ObjectId :=
           PortableServer.ObjectId (Broca.Sequences.Null_Sequence);
      end record;

   procedure Compute_New_Size
     (Buffer : in out Broca.Buffers.Buffer_Descriptor;
      Value  : in Skeleton);

   procedure Marshall
     (Buffer : in out Broca.Buffers.Buffer_Descriptor;
      Value  : in Skeleton);

   type Skeleton_Ptr is access all Skeleton;

   --  Can raise bad_param.
   function To_Skeleton (Ref : CORBA.Object.Ref'Class)
                         return Skeleton_Ptr;

   -----------
   --  POA  --
   -----------

   --  Lock policy.
   --  lock_W when modifying an entry (register_POA and unregister_POA).  Only
   --    in this case a POA can be destroyed.
   --  It is W-locked during creation or destruction of a POA.
   --  lock_R when an access to a POA is used.
   --  It is R-locked looking for the POA from an objectId.
   All_POAs_Lock : Broca.Locks.Rw_Lock_Type;

   type POA_Index_Type is new Natural;
   Bad_Poa_Index : constant POA_Index_Type := 0;
   Root_POA_Index : constant POA_Index_Type := 1;

   type Poa_Object is abstract new Broca.Refs.Ref_Type with
      record
         --  A queue during creation by a poa activator adapter.
         Creation_Lock : Broca.Locks.Bcast_Lock_Type;

         --  This index is set by register_POA.
         Index : POA_Index_Type;

         --  Internal data.
         POA_Manager : POAManager_Object_Ptr;
         Name : CORBA.String;
         Activator : PortableServer.AdapterActivator.Ref;
         Default_Servant : PortableServer.Servant := null;
         Servant_Manager : PortableServer.ServantManager.Ref;

         --  Policies
         Thread_Policy : ThreadPolicyValue;
         Lifespan_Policy : LifespanPolicyValue;
         Uniqueness_Policy : IdUniquenessPolicyValue;
         Id_Assign_Policy : IdAssignmentPolicyValue;
         Servant_Policy : ServantRetentionPolicyValue;
         Request_Policy : RequestProcessingPolicyValue;
         Activation_Policy : ImplicitActivationPolicyValue;

         --  When link_lock is taken for read immediate children cannot be
         --  destroyed, because links field bellow can't be modified.
         Link_Lock : Broca.Locks.Rw_Lock_Type;

         --  Links to build the tree of POA.
         --
         --  Any access to the single linked list of children is protected by
         --  all_poa_lock.
         --  CHILDREN is protected by LINK_LOCK.
         Children : POA_Object_Ptr := null;
         --  BROTHER is under the control of the parent.
         Brother : POA_Object_Ptr := null;
         --  PARENT can't be changed, it is assigned at initialisation.
         Parent : POA_Object_Ptr := null;
      end record;

   --  Note: for all primitives of POA_Object, policies checking is done
   --  by PortableServer operations.

   function Activate_Object (Self : access POA_Object; P_Servant : Servant)
                             return ObjectId is abstract;

   procedure Activate_Object_With_Id
     (Self : access POA_Object;
      Oid : ObjectId;
      P_Servant : PortableServer.Servant) is abstract;

   function Create_Reference
     (Self : access POA_Object; Intf : CORBA.RepositoryId)
      return CORBA.Object.Ref is abstract;

   function Create_Reference_With_Id
     (Self : access POA_Object; Oid : ObjectId; Intf : CORBA.RepositoryId)
      return CORBA.Object.Ref is abstract;

   function Servant_To_Id (Self : access POA_Object; P_Servant : Servant)
                           return ObjectId is abstract;

   function Servant_To_Skeleton (Self : access POA_Object; P_Servant : Servant)
      return Broca.POA.Skeleton_Ptr is abstract;

   function Skeleton_To_Servant
     (Self : access POA_Object; Skeleton : Broca.POA.Skeleton_Ptr)
     return Servant is abstract;

   function Id_To_Skeleton (Self : access POA_Object; Oid : ObjectId)
     return Skeleton_Ptr is abstract;

   procedure Deactivate_Object (Self : access POA_Object; Oid : ObjectId)
     is abstract;

   --  Called by the POAManager to etherealize all objects.
   --  Currently, called only if RETAIN and USE_SERVANT_MANAGER.
   procedure Deactivate (Self : access POA_Object) is abstract;

   --  Return True if the message must be sent, otherwise, it was queued by
   --  the POA.
   --  Requests_lock must have been already lock_r and is unlock_R before
   --  returning.
   procedure GIOP_Invoke
     (Self : access POA_Object;
      Key : in out Broca.Buffers.Buffer_Descriptor;
      Operation : CORBA.Identifier;
      Request_Id : CORBA.Unsigned_Long;
      Reponse_Expected : CORBA.Boolean;
      Message : in out Broca.Buffers.Buffer_Descriptor) is abstract;

   function Get_The_POAManager (Self : access POA_Object)
                                return POAManager_Object_Ptr;

   --  Before calling CREATE_POA,  All_POAs_lock must have been lock_W, so that
   --  links can be safely.  After the call, it is still locked.
   function Create_POA
     (Self         : access POA_Object;
      Adapter_Name : CORBA.String;
      A_POAManager : POAManager_Object_Ptr;
      Tp           : ThreadPolicyValue;
      Lp : LifespanPolicyValue;
      Up : IdUniquenessPolicyValue;
      Ip : IdAssignmentPolicyValue;
      Ap : ImplicitActivationPolicyValue;
      Sp : ServantRetentionPolicyValue;
      Rp : RequestProcessingPolicyValue)
     return POA_Object_Ptr is abstract;

   --  Return null if the POA was not found nor created.
   --  Before calling FIND_POA,  All_POAs_lock must have been lock_W, so that
   --  links can be safely.  After the call, it is still locked, but
   --  it can be unlocked during the processing, if an adapter activator is
   --  called.
   function Find_POA
     (Self         : access POA_Object;
      Adapter_Name : CORBA.String;
      Activate_It  : CORBA.Boolean)
      return POA_Object_Ptr is abstract;

   --  Note: All_POAs_lock must not have been taken.
   procedure Destroy_POA (Self : access POA_Object;
                          Etherealize_Objects : CORBA.Boolean;
                          Wait_For_Completion : CORBA.Boolean) is abstract;

   procedure Cleanup (Self : access POA_Object) is abstract;
end Broca.POA;
