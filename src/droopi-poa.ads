--  Abstract interface for the POA.

--  $Id$

with Droopi.Locks;

with Droopi.POA_Types;     use Droopi.POA_Types;
with Droopi.POA_Manager;

with CORBA;
--  with CORBA.Policy_Values; use CORBA.Policy_Values;
with CORBA.Object_Map;

with Droopi.Types;

with Droopi.POA_Policies;
with Droopi.POA_Policies.Thread_Policy;
with Droopi.POA_Policies.Request_Processing_Policy;
with Droopi.POA_Policies.Id_Assignment_Policy;
with Droopi.POA_Policies.Id_Uniqueness_Policy;
with Droopi.POA_Policies.Servant_Retention_Policy;
with Droopi.POA_Policies.Lifespan_Policy;
with Droopi.POA_Policies.Implicit_Activation_Policy;
use Droopi.POA_Policies.Thread_Policy;
use Droopi.POA_Policies.Request_Processing_Policy;
use Droopi.POA_Policies.Id_Assignment_Policy;
use Droopi.POA_Policies.Id_Uniqueness_Policy;
use Droopi.POA_Policies.Servant_Retention_Policy;
use Droopi.POA_Policies.Lifespan_Policy;
use Droopi.POA_Policies.Implicit_Activation_Policy;

with POA_Configuration;

package Droopi.POA is

   --  Unit has no proper body: no elab control necessary.

   Invalid_Object_Id : exception renames Droopi.POA_Types.Invalid_Object_Id;
   Invalid_Method    : exception renames Droopi.POA_Types.Invalid_Method;

   type Obj_Adapter is abstract new Droopi.POA_Types.Obj_Adapter with
      record
         Name                       : Types.String;
         POA_Manager                : Droopi.POA_Manager.POAManager_Access;
         Boot_Time                  : Time_Stamp;
         Absolute_Address           : Types.String;
         Active_Object_Map          : CORBA.Object_Map.Object_Map_Access;

         --  Configuration
         Configuration              : POA_Configuration.Configuration_Access;

         --  Policies (one of each is required)
         Thread_Policy              : ThreadPolicy_Access             := null;
         Request_Processing_Policy  : RequestProcessingPolicy_Access  := null;
         Id_Assignment_Policy       : IdAssignmentPolicy_Access      := null;
         Id_Uniqueness_Policy       : IdUniquenessPolicy_Access       := null;
         Servant_Retention_Policy   : ServantRetentionPolicy_Access   := null;
         Lifespan_Policy            : LifespanPolicy_Access           := null;
         Implicit_Activation_Policy : ImplicitActivationPolicy_Access := null;

         --  Siblings
         Father                     : Obj_Adapter_Access := null;
         Children                   : POAList_Access     := null;

         --  Locks
         Children_Lock              : Droopi.Locks.Rw_Lock_Access;
         Map_Lock                   : Droopi.Locks.Rw_Lock_Access;
      end record;
   type Obj_Adapter_Access is access all Obj_Adapter'Class;
   --  The POA object
   --  ??? Part of this should be private (locks, active object map, father...)
   --  The policies are used by all corba-policy-*, we can keep them public

   --------------------------------------------------
   --  Procedures and functions required by CORBA  --
   --------------------------------------------------

   function Create_POA
     (Self         : access Obj_Adapter;
      Adapter_Name :        Types.String;
      A_POAManager :        POA_Manager.POAManager_Access;
      Policies     :        Droopi.POA_Policies.PolicyList_Access)
     return Obj_Adapter_Access
      is abstract;
   --  Create a POA given its name and a list of policies
   --  Policies are optionnal : defaults values are provided

   procedure Destroy
     (Self                : access Obj_Adapter;
      Etherealize_Objects : in     Boolean;
      Wait_For_Completion : in     Boolean)
      is abstract;
   --  Destroys recursively the POA and all his descendants

--    function Create_Thread_Policy
--      (Self  : access Obj_Adapter;
--       Value :        ThreadPolicyValue)
--      return ThreadPolicy_Access
--       is abstract;

--    function Create_Lifespan_Policy
--      (Self  : access Obj_Adapter;
--       Value :        LifespanPolicyValue)
--      return LifespanPolicy_Access
--       is abstract;

--    function Create_Id_Uniqueness_Policy
--      (Self  : access Obj_Adapter;
--       Value :        IdUniquenessPolicyValue)
--      return IdUniquenessPolicy_Access
--       is abstract;

--    function Create_Id_Assignment_Policy
--      (Self  : access Obj_Adapter;
--       Value :        IdAssignmentPolicyValue)
--      return IdAssignmentPolicy_Access
--      is abstract;

--    function Create_Servant_Retention_Policy
--      (Self  : access Obj_Adapter;
--       Value :        ServantRetentionPolicyValue)
--      return ServantRetentionPolicy_Access
--      is abstract;

--    function Create_Request_Processing_Policy
--      (Self  : access Obj_Adapter;
--       Value :        RequestProcessingPolicyValue)
--      return RequestProcessingPolicy_Access
--      is abstract;

--    function Create_Implicit_Activation_Policy
--      (Self  : access Obj_Adapter;
--       Value :        ImplicitActivationPolicyValue)
--      return ImplicitActivationPolicy_Access
--       is abstract;

   function Activate_Object
     (Self      : access Obj_Adapter;
      P_Servant : in     Servant_Access)
     return Object_Id
      is abstract;
   --  Activates an object

   procedure Activate_Object_With_Id
     (Self      : access Obj_Adapter;
      P_Servant : in     Servant_Access;
      Oid       : in     Object_Id)
      is abstract;
   --  Activates an object with a specified Id

   procedure Deactivate_Object
     (Self : access Obj_Adapter;
      Oid  : in     Object_Id)
      is abstract;
   --  Deactivates an object from the Active Object Map (requires the RETAIN
   --  policy). In case a ServantManager is used, calls its etherealize
   --  method.
   --  Active requests should be completed before the object is removed
   --  ??? How do we implement that? How do we implement the queue?

   function Servant_To_Id
     (Self      : access Obj_Adapter;
      P_Servant : in     Servant_Access)
     return Object_Id is abstract;
   --  Requires USE_DEFAULT_SERVANT or RETAIN and either UNIQUE_ID
   --  or IMPLICIT_ACTIVATION
   --  Case RETAIN and UNIQUE_ID:
   --    Looks in the object map for the Id of the given servant
   --  Case RETAIN and IMPLICIT_ACTIVATION:
   --    The servant is activated and its Id is returned
   --  Case USE_DEFAULT_SERVANT:
   --    If the servant is not found in the Active Object Map,
   --    the Id of the current invocation is returned.
   --    ???
   --  Otherwise:
   --    Raises a ServantNotActive exception

   function Id_To_Servant
     (Self : access Obj_Adapter;
      Oid  :        Object_Id)
     return Servant_Access is abstract;
   --  Requires RETAIN or USE_DEFAULT_SERVANT
   --  Case RETAIN:
   --    Look for the given Object_Id in the Active Object Map.
   --    If found, returns the associated servant.
   --  Case USE_DEFAULT_SERVANT:
   --    If the Object_Id is not in the map, or the NON_RETAIN policy
   --    is used, returns the default servant (if one has been registered).
   --  Otherwise:
   --    Raises ObjectNotActive

   -------------------------------------------------------
   --  Functions and procedures not in the CORBA Norme  --
   -------------------------------------------------------

   procedure Copy_Obj_Adapter
     (From : in     Obj_Adapter;
      To   : access Obj_Adapter)
      is abstract;
   --  Copy values from one Obj_Adapter to another
   --  (Obj_Adapter is limited...)

   procedure Remove_POA_By_Name
     (Self       : access Obj_Adapter;
      Child_Name :        Types.String)
     is abstract;

end Droopi.POA;
