with Ada.Unchecked_Deallocation;

with Droopi.Objects;
with Droopi.Any;
with Droopi.Any.NVList;
with Droopi.Requests;

with CORBA.Policy;
with CORBA.Policy_Values; use CORBA.Policy_Values;
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

package CORBA.POA.Basic_POA is

   type Basic_Obj_Adapter is new CORBA.POA.Obj_Adapter with
      record
         P_Factory : CORBA.Policy.Policies_Factory;
      end record;
   type Basic_Obj_Adapter_Access is access all Basic_Obj_Adapter;
   --  The POA object

   --------------------------------------------------
   --  Procedures and functions required by CORBA  --
   --------------------------------------------------

   function Create_POA
     (Self         : access Basic_Obj_Adapter;
      Adapter_Name :        String;
      A_POAManager :        POA_Manager.POAManager_Access;
      Policies     :        Policy.PolicyList_Access)
     return Obj_Adapter_Access;
   --  Create a POA given its name and a list of policies
   --  Policies are optionnal : defaults values are provided

   procedure Destroy
     (Self                : access Basic_Obj_Adapter;
      Etherealize_Objects : in     Boolean;
      Wait_For_Completion : in     Boolean);

   function Create_Thread_Policy
     (Self  : access Basic_Obj_Adapter;
      Value :        ThreadPolicyValue)
     return ThreadPolicy_Access;

   function Create_Lifespan_Policy
     (Self  : access Basic_Obj_Adapter;
      Value :        LifespanPolicyValue)
     return LifespanPolicy_Access;

   function Create_Id_Uniqueness_Policy
     (Self  : access Basic_Obj_Adapter;
      Value :        IdUniquenessPolicyValue)
     return IdUniquenessPolicy_Access;

   function Create_Id_Assignement_Policy
     (Self  : access Basic_Obj_Adapter;
      Value :        IdAssignementPolicyValue)
     return IdAssignementPolicy_Access;

   function Create_Servant_Retention_Policy
     (Self  : access Basic_Obj_Adapter;
      Value :        ServantRetentionPolicyValue)
     return ServantRetentionPolicy_Access;

   function Create_Request_Processing_Policy
     (Self  : access Basic_Obj_Adapter;
      Value :        RequestProcessingPolicyValue)
     return RequestProcessingPolicy_Access;

   function Create_Implicit_Activation_Policy
     (Self  : access Basic_Obj_Adapter;
      Value :        ImplicitActivationPolicyValue)
     return ImplicitActivationPolicy_Access;

   function Activate_Object
     (Self      : access Basic_Obj_Adapter;
      P_Servant : in     Servant_Access)
     return Object_Id;

   procedure Activate_Object_With_Id
     (Self      : access Basic_Obj_Adapter;
      P_Servant : in     Servant_Access;
      Oid       : in     Object_Id);

   procedure Deactivate_Object
     (Self      : access Basic_Obj_Adapter;
      Oid       : in Object_Id);

   function Servant_To_Id
     (Self      : access Basic_Obj_Adapter;
      P_Servant : in     Servant_Access)
     return Object_Id;

   function Id_To_Servant
     (Self : access Basic_Obj_Adapter;
      Oid  :        Object_Id)
     return Servant_Access;

   --------------------------------------------------------
   --  Functions and procedures to interface with Droopi --
   --------------------------------------------------------

   procedure Create
     (OA : access Basic_Obj_Adapter);

   procedure Destroy
     (OA : access Basic_Obj_Adapter);

   function Export
     (OA  : access Basic_Obj_Adapter;
      Obj :        Droopi.Objects.Servant_Access)
     return Droopi.Objects.Object_Id;

   procedure Unexport
     (OA : access Basic_Obj_Adapter;
      Id :        Droopi.Objects.Object_Id);

   function Get_Empty_Arg_List
     (OA     : access Basic_Obj_Adapter;
      Oid    : Droopi.Objects.Object_Id;
      Method : Droopi.Requests.Operation_Id)
     return Droopi.Any.NVList.Ref;

   function Get_Empty_Result
     (OA     : access Basic_Obj_Adapter;
      Oid    : Droopi.Objects.Object_Id;
      Method : Droopi.Requests.Operation_Id)
     return Droopi.Any.Any;

   function Find_Servant
     (OA : access Basic_Obj_Adapter;
      Id :        Droopi.Objects.Object_Id)
     return Droopi.Objects.Servant_Access;

   procedure Release_Servant
     (OA      : access Basic_Obj_Adapter;
      Id      :        Droopi.Objects.Object_Id;
      Servant : in out Droopi.Objects.Servant_Access);

   -------------------------------------------------
   --  Utilities, neither in CORBA nor in Droopi  --
   -------------------------------------------------

   procedure Copy_Obj_Adapter
     (From : in     Basic_Obj_Adapter;
      To   : access Basic_Obj_Adapter);

   procedure Remove_POA_By_Name
     (Self       : access Basic_Obj_Adapter;
      Child_Name :        String);
   --  Remove a child POA from Self's list of children
   --  Doesn't lock the list of children

   function Create_Root_POA
     return Obj_Adapter_Access;
   --  ??? Should be private ; access is possible through Create

   function Find_POA_Recursively
     (Self : access Basic_Obj_Adapter;
      Name :        String)
     return Basic_Obj_Adapter_Access;
   --  Starting from given POA, looks for the POA in all the descendancy whose
   --  name is Name. Returns null if not found.
   --  ??? Should be private

   procedure Free is new Ada.Unchecked_Deallocation
     (Basic_Obj_Adapter, Basic_Obj_Adapter_Access);

private
   type Check_State is (CHECK, NO_CHECK);

end CORBA.POA.Basic_POA;
