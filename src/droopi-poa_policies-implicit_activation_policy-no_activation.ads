with Ada.Unchecked_Deallocation;

package Droopi.POA_Policies.Implicit_Activation_Policy.No_Activation is

   type No_Activation_Policy is new ImplicitActivationPolicy with null record;
   type No_Activation_Policy_Access is access all No_Activation_Policy;

   function Create return No_Activation_Policy_Access;
   procedure Check_Compatibility (Self : No_Activation_Policy;
                                  OA   : Droopi.POA_Types.Obj_Adapter_Access);

   function Activate_Servant (Self      : No_Activation_Policy;
                              OA        : Droopi.POA_Types.Obj_Adapter_Access;
                              P_Servant : Servant_Access)
                             return Object_Id_Access;

   procedure Free (P   : in     No_Activation_Policy;
                   Ptr : in out Policy_Access);

   procedure Free is
      new Ada.Unchecked_Deallocation (No_Activation_Policy,
                                      No_Activation_Policy_Access);

end Droopi.POA_Policies.Implicit_Activation_Policy.No_Activation;
