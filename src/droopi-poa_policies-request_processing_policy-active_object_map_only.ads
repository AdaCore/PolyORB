--  with CORBA.Policy_Values; use CORBA.Policy_Values;
--  with Droopi.POA_Types;     use Droopi.POA_Types;
with Ada.Unchecked_Deallocation;

package Droopi.POA_Policies.Request_Processing_Policy.Active_Object_Map_Only is

   type Active_Map_Only_Policy is new RequestProcessingPolicy with null record;
   type Active_Map_Only_Policy_Access is access all Active_Map_Only_Policy;

   function Create return Active_Map_Only_Policy_Access;
   procedure Check_Compatibility (Self : Active_Map_Only_Policy;
                                  OA   : Droopi.POA_Types.Obj_Adapter_Access);

   procedure Etherealize_All (Self  : Active_Map_Only_Policy;
                              OA    : Droopi.POA_Types.Obj_Adapter_Access;
                              U_Oid : Unmarshalled_Oid_Access);

   function Servant_To_Id (Self  : Active_Map_Only_Policy;
                           OA    : Droopi.POA_Types.Obj_Adapter_Access;
                           P_Servant : Servant_Access) return Object_Id_Access;


   function Id_To_Servant (Self : Active_Map_Only_Policy;
                           OA   : Droopi.POA_Types.Obj_Adapter_Access;
                           Oid  : Object_Id) return Servant_Access;

   procedure Free (P   : in     Active_Map_Only_Policy;
                   Ptr : in out Policy_Access);

   procedure Free is
      new Ada.Unchecked_Deallocation (Active_Map_Only_Policy,
                                      Active_Map_Only_Policy_Access);

end Droopi.POA_Policies.Request_Processing_Policy.Active_Object_Map_Only;
