with Ada.Unchecked_Deallocation;

package Droopi.POA_Policies.Lifespan_Policy.Transient is

   type Transient_Policy is new LifespanPolicy with null record;
   type Transient_Policy_Access is access all Transient_Policy;

   function Create return Transient_Policy_Access;
   procedure Check_Compatibility (Self : Transient_Policy;
                                  OA   : Droopi.POA_Types.Obj_Adapter_Access);

   function Get_Time_Stamp (P  : Transient_Policy;
                            OA : Droopi.POA_Types.Obj_Adapter_Access)
                           return Time_Stamp;

   procedure Ensure_Lifespan (P     : Transient_Policy;
                              OA    : Droopi.POA_Types.Obj_Adapter_Access;
                              U_Oid : Unmarshalled_Oid_Access);

   procedure Free
     (P   : in     Transient_Policy;
      Ptr : in out Policy_Access);

   procedure Free is new Ada.Unchecked_Deallocation
     (Transient_Policy,
      Transient_Policy_Access);

end Droopi.POA_Policies.Lifespan_Policy.Transient;
