with PolyORB.POA_Types;               use PolyORB.POA_Types;

package PolyORB.POA_Policies.Id_Uniqueness_Policy is

   type IdUniquenessPolicy is abstract new Policy with null record;
   subtype Id_Uniqueness_Policy is IdUniquenessPolicy;
   type IdUniquenessPolicy_Access is access all IdUniquenessPolicy'Class;
   subtype Id_Uniqueness_Policy_Access is IdUniquenessPolicy_Access;

   function Create return IdUniquenessPolicy_Access is abstract;
   --  The real creation function that has to be implemented for each
   --  possible Policy

   procedure Free (P   : in     IdUniquenessPolicy;
                   Ptr : in out Policy_Access)
      is abstract;

   procedure Ensure_Servant_Uniqueness
     (Self      : IdUniquenessPolicy;
      OA        : PolyORB.POA_Types.Obj_Adapter_Access;
      P_Servant : Servant_Access)
     is abstract;
   --  Case UNIQUE_ID:
   --  Checks that the specified servant is not yet in the Active Objects Map.
   --  If not, throws a ServantAlreadyActive exception.
   --  Case MULTIPLE_ID:
   --  Does nothing

   function Servant_To_Id (Self      : IdUniquenessPolicy;
                           OA        : PolyORB.POA_Types.Obj_Adapter_Access;
                           P_Servant : Servant_Access) return Object_Id_Access
      is abstract;
   --  Case UNIQUE_ID:
   --    Looks for the specified servant in the Active Object Map.
   --    If found, returns its Object_Id.
   --    Otherwise, returns null.
   --  Case MULTIPLE_ID:
   --    Returns null.

end PolyORB.POA_Policies.Id_Uniqueness_Policy;
