with Ada.Unchecked_Deallocation;

package CORBA.Policy.Implicit_Activation_Policy.No_Activation is

   type No_Activation_Policy is new ImplicitActivationPolicy with null record;
   type No_Activation_Policy_Access is access all No_Activation_Policy;

   function Create return No_Activation_Policy_Access;
   procedure Check_Compatibility (Self : No_Activation_Policy;
                                  OA   : CORBA.POA_Types.Obj_Adapter_Access);

   procedure Free (P   : in     No_Activation_Policy;
                   Ptr : in out Policy_Access);

   procedure Free is
      new Ada.Unchecked_Deallocation (No_Activation_Policy,
                                      No_Activation_Policy_Access);

end CORBA.Policy.Implicit_Activation_Policy.No_Activation;
