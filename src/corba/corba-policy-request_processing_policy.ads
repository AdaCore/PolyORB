with CORBA.Policy_Values; use CORBA.Policy_Values;
with CORBA.POA_Types;     use CORBA.POA_Types;
with Droopi.Objects;      use Droopi.Objects;

package CORBA.Policy.Request_Processing_Policy is

   type Request_Processing_Policy is abstract new Policy with private;
   type Request_Processing_Policy_Ptr is access all Request_Processing_Policy;

   function Servant_To_Id
     (Self             : access Request_Processing_Policy;
      OA               : access CORBA.POA_Types.Obj_Adapter;
      P_Servant        : in Servant)
     return Object_Id is abstract;
   --  Case USE_ACTIVE_OBJECT_MAP_ONLY:
   --  Look for the given servant and returns its Id
   --  Requires the RETAIN policy
   --  Case USE_DEFAULT_SERVANT:
   --  In case the given servant is not found in the object map,
   --  returns the Id of the default servant.
   --  Case USE_SERVANT_MANAGER:
   --  Same than USE_ACTIVE_OBJECT_MAP_ONLY

   function Id_To_Servant
     (Self             : access Request_Processing_Policy;
      OA               : access CORBA.POA_Types.Obj_Adapter;
      Oid              : Object_Id)
     return Servant is abstract;
   --  Case USE_ACTIVE_OBJECT_MAP_ONLY:
   --  Look for the given Id in the map and returns its associated servant
   --  Case USE_DEFAULT_SERVANT:
   --  If the Id is not found in the map, returns the default servant
   --  Case USE_SERVANT_MANAGER:
   --  Calls the ServantManager to create the servant if not found in the map
   --  ????


private
   type Request_Processing_Policy is abstract new Policy with
      record
         Value : RequestProcessingPolicyValue;
      end record;


end CORBA.Policy.Request_Processing_Policy;
