with Droopi.POA_Types;

package Droopi.POA_Policies.Thread_Policy is

   type ThreadPolicy is abstract new Policy with null record;
   subtype Thread_Policy is ThreadPolicy;
   type ThreadPolicy_Access is access all ThreadPolicy'Class;
   subtype Thread_Policy_Access is ThreadPolicy_Access;

   function Create return ThreadPolicy_Access is abstract;
   --  The real creation function that has to be implemented for each
   --  possible Policy

   procedure Check_Compatibility (Self : ThreadPolicy;
                                  OA   : Droopi.POA_Types.Obj_Adapter_Access)
      is abstract;

   procedure Free (P   : in     ThreadPolicy;
                   Ptr : in out Policy_Access)
      is abstract;

end Droopi.POA_Policies.Thread_Policy;
