package body Droopi.POA_Policies.Implicit_Activation_Policy.No_Activation is

   ------------
   -- Create --
   ------------

   function Create return No_Activation_Policy_Access is
   begin
      return new No_Activation_Policy;
   end Create;

   -------------------------
   -- Check_Compatibility --
   -------------------------

   procedure Check_Compatibility
     (Self : No_Activation_Policy;
      OA   : Droopi.POA_Types.Obj_Adapter_Access)
   is
   begin
      null;
   end Check_Compatibility;

   ---------------
   -- Policy_Id --
   ---------------

   function Policy_Id
     (Self : No_Activation_Policy)
     return String is
   begin
      return "IMPLICIT_ACTIVATION_POLICY.NO_ACTIVATION";
   end Policy_Id;

   ----------------------
   -- Activate_Servant --
   ----------------------

   function Activate_Servant
     (Self      : No_Activation_Policy;
      OA        : Droopi.POA_Types.Obj_Adapter_Access;
      P_Servant : Servant_Access)
     return Object_Id_Access
   is
   begin
      return null;
   end Activate_Servant;

   ----------
   -- Free --
   ----------

   procedure Free (P   : in     No_Activation_Policy;
                   Ptr : in out Policy_Access)
   is
   begin
      Free (No_Activation_Policy_Access (Ptr));
   end Free;

end Droopi.POA_Policies.Implicit_Activation_Policy.No_Activation;
