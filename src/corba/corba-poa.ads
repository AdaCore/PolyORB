with Droopi.Obj_Adapters;
with Droopi.Objects;                use Droopi.Objects;

with CORBA.POA_Manager;
with CORBA.Policy_List;             use CORBA.Policy_List;
--  Policy_List is the declaration of type PolicyList and
--  instantation of a generic package with this type.

with CORBA.POA_Types; use CORBA.POA_Types;

with CORBA.Policy.Thread_Policy;
use  CORBA.Policy.Thread_Policy;
with CORBA.Policy.Request_Processing_Policy;
use  CORBA.Policy.Request_Processing_Policy;
with CORBA.Policy.Id_Assignement_Policy;
use  CORBA.Policy.Id_Assignement_Policy;
with CORBA.Policy.Id_Uniqueness_Policy;
use  CORBA.Policy.Id_Uniqueness_Policy;
with CORBA.Policy.Servant_Retention_Policy;
use  CORBA.Policy.Servant_Retention_Policy;
with CORBA.Policy.Lifespan_Policy;
use  CORBA.Policy.Lifespan_Policy;
with CORBA.Policy.Implicit_Activation_Policy;
use  CORBA.Policy.Implicit_Activation_Policy;

package CORBA.POA is
   --  Declaration of types
   --    type Corba_Obj_Adapter is abstract new Droopi.Obj_Adapters.Obj_Adapter
   --      with private;
   --    type Corba_Obj_Adapter_Ptr is access Droopi.Obj_Adapters.Obj_Adapter;

   function Create_POA
     (Self         : access CORBA.POA_Types.Obj_Adapter;
      Adapter_Name : in CORBA.String;
      A_POAManager : in CORBA.POA_Manager.POA_Manager_Ptr;
      Policies : in CORBA.Policy_List.Policy_List)
     return CORBA.POA_Types.Obj_Adapter_Ptr is abstract;
   --  Create a POA given its name and a list of policies
   --  Policies are optionnal : defaults values are provided

   function Find_POA
     (Self         : access CORBA.POA_Types.Obj_Adapter;
      Adapter_Name : in CORBA.String;
      Activate_It  : in CORBA.Boolean)
     return CORBA.POA_Types.Obj_Adapter is abstract;
   --  Use brothers and children to find a poa given its name

   procedure Destroy
     (Self                : access CORBA.POA_Types.Obj_Adapter;
      Etherealize_Objects : in CORBA.Boolean;
      Wait_For_Completion : in CORBA.Boolean)
      is abstract;

   --  Add here the factories for the policies

   --  Add the procedures for default servant and servant manager

   function Activate_Object
     (Self        : access CORBA.POA_Types.Obj_Adapter;
      P_Servant   : in Servant)
     return Object_Id is abstract;
   --  Activates an object

   procedure Activate_Object_With_Id
     (Self        : access CORBA.POA_Types.Obj_Adapter;
      Oid         : in Object_Id;
      P_Servant   : in Servant)
     is abstract;
   --  Activates an object with a specified Object_Id

   procedure Deactivate_Object
     (Self        : access CORBA.POA_Types.Obj_Adapter;
      Oid         : in Object_Id)
      is abstract;
   --  Deactivate an object

   function Servant_To_Id
     (Self        : access CORBA.POA_Types.Obj_Adapter;
      P_Servant   : in Servant)
     return Object_Id is abstract;
   --  Requires USE_DEFAULT_SERVANT or RETAIN and either UNIQUE_ID
   --  or IMPLICIT_ACTIVATION
   --  Case RETAIN and UNIQUE_ID:
   --  Looks in the object map for the Id of the given servant
   --  Case RETAIN and IMPLICIT_ACTIVATION:
   --  The servant is activated and its Id is returned
   --  Case USE_DEFAULT_SERVANT:
   --  The Id of the current invocation is returned
   --  Otherwise: raises a ServantNotActive exception

   function Id_To_Servant
     (Self        : access CORBA.POA_Types.Obj_Adapter;
      Oid         : Object_Id)
     return Servant is abstract;
   --  Requires either RETAIN or USE_DEFAULT_SERVANT
   --  Case RETAIN:
   --  Looks in the object map for the given Id, and returns the associated
   --  servant
   --  Case USE_DEFAULT_SERVANT:
   --  If the Id is not found in the object map, returns the default servant
   --  Otherwise:
   --  Raises a WrongPolicy exception

end CORBA.POA;
