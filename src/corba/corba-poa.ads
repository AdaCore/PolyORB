with Droopi.Obj_Adapters;
with Droopi.Requests;
with Droopi.Locks;

with CORBA.NVList;
with CORBA.POA_Types;               use CORBA.POA_Types;
with CORBA.POA_Manager;
with CORBA.Object_Map;
with CORBA.Policy;
with CORBA.Policy.Thread_Policy;
with CORBA.Policy.Request_Processing_Policy;
with CORBA.Policy.Id_Assignement_Policy;
with CORBA.Policy.Id_Uniqueness_Policy;
with CORBA.Policy.Servant_Retention_Policy;
with CORBA.Policy.Lifespan_Policy;
with CORBA.Policy.Implicit_Activation_Policy;
use CORBA.Policy.Thread_Policy;
use CORBA.Policy.Request_Processing_Policy;
use CORBA.Policy.Id_Assignement_Policy;
use CORBA.Policy.Id_Uniqueness_Policy;
use CORBA.Policy.Servant_Retention_Policy;
use CORBA.Policy.Lifespan_Policy;
use CORBA.Policy.Implicit_Activation_Policy;

package CORBA.POA is

   type Obj_Adapter is abstract new CORBA.POA_Types.Obj_Adapter with
      record
         Name                       : CORBA.String;
         POA_Manager                : CORBA.POA_Manager.POAManager_Access;
         Boot_Time                  : Time_Stamp;
         Absolute_Address           : String;
         Active_Object_Map          : CORBA.Object_Map.Object_Map_Access;
         --  ??? Access?

         --  Policies (one of each is required)
         Thread_Policy              : ThreadPolicy_Access             := null;
         Request_Processing_Policy  : RequestProcessingPolicy_Access  := null;
         Id_Assignement_Policy      : IdAssignementPolicy_Access      := null;
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

   function Create_POA
     (Self         : access Obj_Adapter;
      Adapter_Name :        String;
      A_POAManager :        POA_Manager.POAManager_Access;
      Policies     :        Policy.PolicyList_Access)
     return Obj_Adapter_Access
      is abstract;
   --  Create a POA given its name and a list of policies
   --  Policies are optionnal : defaults values are provided

--    function Find_POA
--      (Self         : access CORBA.POA_Types.Obj_Adapter;
--       Adapter_Name : in CORBA.String;
--       Activate_It  : in CORBA.Boolean)
--      return CORBA.POA_Types.Obj_Adapter is abstract;
--    --  Given a POA name, looks for the poa and return it if found

--    procedure Destroy
--      (Self                : access CORBA.POA_Types.Obj_Adapter;
--       Etherealize_Objects : in CORBA.Boolean;
--       Wait_For_Completion : in CORBA.Boolean)
--       is abstract;

--    --  Add here the factories for the policies

--    --  Add the procedures for default servant and servant manager

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

   procedure Deactivate
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

end CORBA.POA;
