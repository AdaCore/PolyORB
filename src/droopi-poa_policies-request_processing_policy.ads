with Droopi.POA_Types;     use Droopi.POA_Types;

package Droopi.POA_Policies.Request_Processing_Policy is

   type RequestProcessingPolicy is abstract new Policy with null record;
   subtype Request_Processing_Policy is RequestProcessingPolicy;
   type RequestProcessingPolicy_Access is
     access all RequestProcessingPolicy'Class;
   subtype Request_Processing_Policy_Access is RequestProcessingPolicy_Access;

   function Create return RequestProcessingPolicy_Access is abstract;
   --  The real creation function that has to be implemented for each
   --  possible Request Processing Policy

   procedure Etherealize_All (Self  : RequestProcessingPolicy;
                              OA    : Droopi.POA_Types.Obj_Adapter_Access;
                              U_Oid : Unmarshalled_Oid_Access)
      is abstract;
   --  If a servant manager is used, etherealize the servant(s) associated
   --  with the given Object_Id.

   procedure Free (P   : in     RequestProcessingPolicy;
                   Ptr : in out Policy_Access)
      is abstract;

   function Servant_To_Id (Self      : RequestProcessingPolicy;
                           OA        : Droopi.POA_Types.Obj_Adapter_Access;
                           P_Servant : Servant_Access) return Object_Id_Access
     is abstract;
   --  Case USE_ACTIVE_OBJECT_MAP_ONLY:
   --    Look for the given servant and returns its Id
   --    Requires the RETAIN policy
   --  Case USE_DEFAULT_SERVANT:
   --    In case the given servant is not found in the object map,
   --    returns the Id of the default servant.
   --  Case USE_SERVANT_MANAGER:
   --    Same than USE_ACTIVE_OBJECT_MAP_ONLY

   function Id_To_Servant (Self : RequestProcessingPolicy;
                           OA   : Droopi.POA_Types.Obj_Adapter_Access;
                           Oid  : Object_Id) return Servant_Access
      is abstract;
   --  Case USE_OBJECT_MAP_ONLY:
   --    Asks the Servant Retention Policy to look for the given Oid.
   --    If NON_RETAIN, raises WrongPolicy.
   --    If found, returns the associated servant.
   --    Otherwise, raises ObjectNotActive
   --  Case USE_DEFAULT_SERVANT:
   --    If not found in the Active Object Map, returns the default servant.
   --    If there's no default servant registered, raises Obj_Adapter with
   --    minor code 3.
   --  Case USE_SERVANT_MANAGER:
   --    If not found in the Active Object Map, asks the servant manager
   --    to create it. If there's not servant manager, raises Obj_Adapter
   --    with minor code 4.

end Droopi.POA_Policies.Request_Processing_Policy;
