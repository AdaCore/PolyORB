------------------------------------------------------------------------------
--                                                                          --
--                          ADABROKER COMPONENTS                            --
--                                                                          --
--                            B R O C A . P O A                             --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 1999-2001 ENST Paris University, France.          --
--                                                                          --
-- AdaBroker is free software; you  can  redistribute  it and/or modify it  --
-- under terms of the  GNU General Public License as published by the  Free --
-- Software Foundation;  either version 2,  or (at your option)  any  later --
-- version. AdaBroker  is distributed  in the hope that it will be  useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License  for more details.  You should have received  a copy of the GNU  --
-- General Public License distributed with AdaBroker; see file COPYING. If  --
-- not, write to the Free Software Foundation, 59 Temple Place - Suite 330, --
-- Boston, MA 02111-1307, USA.                                              --
--                                                                          --
-- As a special exception,  if other files  instantiate  generics from this --
-- unit, or you link  this unit with other files  to produce an executable, --
-- this  unit  does not  by itself cause  the resulting  executable  to  be --
-- covered  by the  GNU  General  Public  License.  This exception does not --
-- however invalidate  any other reasons why  the executable file  might be --
-- covered by the  GNU Public License.                                      --
--                                                                          --
--             AdaBroker is maintained by ENST Paris University.            --
--                     (email: broker@inf.enst.fr)                          --
--                                                                          --
------------------------------------------------------------------------------



with CORBA;
with CORBA.Impl;
with CORBA.AbstractBase;
with CORBA.Object;

with PortableServer; use PortableServer;
with PortableServer.AdapterActivator;
with PortableServer.ServantManager;

with Broca.Buffers;
with Broca.Locks;

package Broca.POA is

   type POA_Object;
   type POA_Object_Ptr is access all POA_Object'Class;

   type Ref is new CORBA.AbstractBase.Ref with private;
   function POA_Object_Of (The_Ref : Ref) return POA_Object_Ptr;
   procedure Set (The_Ref : in out Ref; The_Object : POA_Object_Ptr);

   Nil_Ref : constant Ref;

   ------------------
   --  POAManager  --
   ------------------

   --  9.3.2  Processing States.
   --  A POA manager has four possible processing states: active,
   --  inactive, holding and discarding.
   type Processing_State_Type is (Holding, Active, Discarding, Inactive);

   --  POAManager internal object and primitives.
   type POAManager_Object is abstract new CORBA.Impl.Object
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
                       A_POA : Ref'Class) is abstract;
   procedure Unregister (Self : in out POAManager_Object;
                         A_POA : Ref'Class) is abstract;

   --  9.3.2
   --  A POA Manager encapsulates the processing state of the POAs it is
   --  associed with.
   --
   --  There is also a usage counter associed with the state.  It is used
   --  only to implement wait_for_completion.  However, it is externalized
   --  through these subprograms.
   --
   --  Get state and increment usage counter is the state is active.
   procedure Inc_Usage_If_Active
     (Self : in out POAManager_Object;
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

   ----------------
   --  Skeleton  --
   ----------------

   --  A Skeleton is the internal object for an object implementation.

   type Object_Key_Ptr is access all Broca.Buffers.Encapsulation;

   type Skeleton is new CORBA.Impl.Object with
      record
         Type_Id    : CORBA.RepositoryId;
         Object_Key : Object_Key_Ptr;

         P_Servant  : PortableServer.Servant;

         POA        : Ref;

         --  ObjectId.
         Object_Id : PortableServer.ObjectId
           := PortableServer.ObjectId
             (PortableServer.IDL_SEQUENCE_Octet.Null_Sequence);
      end record;

   procedure Marshall
     (Buffer : access Buffers.Buffer_Type;
      Value  : in Skeleton);

   type Skeleton_Ptr is access all Skeleton;

   function Skeleton_To_Ref
     (Skel : Skeleton)
     return CORBA.Object.Ref;

   --  Can raise Bad_Param.
   function Ref_To_Skeleton
     (Ref : CORBA.Object.Ref'Class)
     return Skeleton_Ptr;

   -----------
   --  POA  --
   -----------

   --  Lock policy.

   --  Lock created by Broca.POA.Start, detroyed by Broca.POA.Stop.
   --  lock_W when modifying an entry (register_POA and unregister_POA).  Only
   --    in this case a POA can be destroyed.
   --  It is W-locked during creation or destruction of a POA.
   --  lock_R when an access to a POA is used.
   --  It is R-locked looking for the POA from an objectId.

   All_POAs_Lock : Broca.Locks.Rw_Lock_Access;

   type POA_Index_Type is new Natural;
   Bad_POA_Index : constant POA_Index_Type := 0;
   Root_POA_Index : constant POA_Index_Type := 1;

   --  type POA_Object is abstract new Broca.Refs.Entity with
   type POA_Object is abstract new CORBA.Impl.Object with
      record
         --  A queue during creation by a poa activator adapter.
         Creation_Lock : Broca.Locks.Bcast_Lock_Type;

         --  This index is set by register_POA.
         Index : POA_Index_Type;

         --  Internal data.

         --  FIXME: POA_Manager should be a Ref.
         POA_Manager     : POAManager_Object_Ptr;
         Name            : CORBA.String;
         Activator       : PortableServer.AdapterActivator.Ref;

         --  FIXME: Default_Servant should be a Ref.
         Default_Servant : PortableServer.Servant := null;
         Servant_Manager : PortableServer.ServantManager.Ref;

         --  Policies
         Thread_Policy     : ThreadPolicyValue;
         Lifespan_Policy   : LifespanPolicyValue;
         Uniqueness_Policy : IdUniquenessPolicyValue;
         Id_Assign_Policy  : IdAssignmentPolicyValue;
         Servant_Policy    : ServantRetentionPolicyValue;
         Request_Policy    : RequestProcessingPolicyValue;
         Activation_Policy : ImplicitActivationPolicyValue;

         --  When link_lock is taken for read immediate children cannot be
         --  destroyed, because links field bellow can't be modified.
         --  Must be created/destroyed by Create_POA/Destroy_POA.
         Link_Lock : Broca.Locks.Rw_Lock_Access;

         --  Links to build the tree of POA.
         --
         --  Any access to the single linked list of children is protected by
         --  all_poa_lock.

         --  FIXME: Should these be changed to Broca.POA.Ref?

         --  CHILDREN is protected by LINK_LOCK.
         Children : POA_Object_Ptr;
         --  BROTHER is under the control of the parent.
         Brother  : POA_Object_Ptr;
         --  PARENT can't be changed, it is assigned at initialisation.
         Parent   : POA_Object_Ptr;
      end record;

   --  Note: for all primitives of POA_Object, policies checking is done
   --  by PortableServer operations.

   function Activate_Object
     (Self      : access POA_Object;
      P_Servant : Servant)
     return ObjectId is abstract;

   procedure Activate_Object_With_Id
     (Self      : access POA_Object;
      Oid       : in ObjectId;
      P_Servant : in PortableServer.Servant) is abstract;

   function Create_Reference
     (Self : access POA_Object;
      Intf : CORBA.RepositoryId)
      return CORBA.Object.Ref is abstract;

   function Create_Reference_With_Id
     (Self : access POA_Object;
      Oid  : ObjectId;
      Intf : CORBA.RepositoryId)
      return CORBA.Object.Ref is abstract;

   function Servant_To_Skeleton
     (Self                             : access POA_Object;
      P_Servant                        : Servant;
      Called_From_Servant_To_Reference : Boolean := False)
     return Skeleton_Ptr is abstract;
   --  Return a skeleton for the given servant.
   --  This is used to implement the Servant_To_Id and
   --  Servant_To_Reference POA operations.

   function Skeleton_To_Servant
     (Self     : access POA_Object;
      Skeleton : Skeleton_Ptr)
     return Servant
      is abstract;

   function Id_To_Skeleton
     (Self : access POA_Object;
      Oid  : in ObjectId)
     return Skeleton_Ptr is abstract;

   function Key_To_Skeleton
     (Self : access POA_Object;
      Key  : in Object_Key_Ptr)
     return Skeleton_Ptr is abstract;

   procedure Deactivate_Object
     (Self : access POA_Object;
      Oid  : in ObjectId) is abstract;

   procedure Deactivate (Self : access POA_Object) is abstract;
   --  Called by the POAManager to etherealize all objects.
   --  Currently, called only if RETAIN and USE_SERVANT_MANAGER.

   procedure GIOP_Invoke
     (Self             : access POA_Object;
      Key              : access Broca.Buffers.Encapsulation;
      Operation        : in CORBA.Identifier;
      Request_Id       : in CORBA.Unsigned_Long;
      Reponse_Expected : CORBA.Boolean;
      Message          : access Broca.Buffers.Buffer_Type;
      Reply            : access Broca.Buffers.Buffer_Type) is abstract;
   --  Return True if the message must be sent, otherwise, it is
   --  queued by the POA. Requests_lock must have been already lock_r
   --  and is unlock_r after the call.

   function Get_The_POAManager
     (Self : access POA_Object)
     return POAManager_Object_Ptr;

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
   --  Before calling Create_Poa, All_POAs_lock must have been lock_W,
   --  so links can be safely.  After the call, it is still locked.

   function Find_POA
     (Self         : access POA_Object;
      Adapter_Name : CORBA.String;
      Activate_It  : CORBA.Boolean)
      return Ref'Class is abstract;
   --  Return null if the POA was not found nor created.  Before
   --  calling Find_Poa, All_POAs_lock must have been lock_W, so that
   --  links can be safely. After the call, it is still locked, but it
   --  can be unlocked during the processing, if an adapter activator
   --  is called.

   procedure Destroy_POA
     (Self                : access POA_Object;
      Etherealize_Objects : in CORBA.Boolean;
      Wait_For_Completion : in CORBA.Boolean) is abstract;
   --  Note: All_POAs_lock must not have been taken.

   procedure Cleanup (Self : access POA_Object) is abstract;

   function To_POA_Ref (The_POA : POA_Object_Ptr) return Ref;

   procedure Start;
   --  Initialize.

   procedure Stop;
   --  Terminate.

private

   type Ref is new CORBA.AbstractBase.Ref with null record;
   Nil_Ref : constant Ref := (CORBA.AbstractBase.Nil_Ref with null record);

end Broca.POA;
