with Ada.Unchecked_Deallocation;

package Droopi.POA_Policies.Id_Uniqueness_Policy.Unique is

   type Unique_Id_Policy is new IdUniquenessPolicy with null record;
   type Unique_Id_Policy_Access is access all Unique_Id_Policy;

   function Create return Unique_Id_Policy_Access;

   procedure Check_Compatibility
     (Self : Unique_Id_Policy;
      OA   : Droopi.POA_Types.Obj_Adapter_Access);

   function Policy_Id
     (Self : Unique_Id_Policy)
     return String;

   procedure Ensure_Servant_Uniqueness
     (Self      : Unique_Id_Policy;
      OA        : Droopi.POA_Types.Obj_Adapter_Access;
      P_Servant : Servant_Access);

   function Servant_To_Id
     (Self      : Unique_Id_Policy;
      OA        : Droopi.POA_Types.Obj_Adapter_Access;
      P_Servant : Servant_Access)
     return Object_Id_Access;

   procedure Free
     (P   : in     Unique_Id_Policy;
      Ptr : in out Policy_Access);

   procedure Free is new Ada.Unchecked_Deallocation
     (Unique_Id_Policy,
      Unique_Id_Policy_Access);

end Droopi.POA_Policies.Id_Uniqueness_Policy.Unique;
