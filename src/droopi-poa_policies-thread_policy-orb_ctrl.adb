package body Droopi.POA_Policies.Thread_Policy.ORB_Ctrl is

   ------------
   -- Create --
   ------------

   function Create return ORB_Ctrl_Policy_Access is
   begin
      return new ORB_Ctrl_Policy;
   end Create;

   ---------------
   -- Policy_Id --
   ---------------

   function Policy_Id
     (Self : ORB_Ctrl_Policy)
     return String is
   begin
      return "THREAD_POLICY.ORB_CTRL";
   end Policy_Id;

   -------------------------
   -- Check_Compatibility --
   -------------------------

   procedure Check_Compatibility
     (Self : ORB_Ctrl_Policy;
      OA   : Droopi.POA_Types.Obj_Adapter_Access)
   is
   begin
      null;
   end Check_Compatibility;

   ----------
   -- Free --
   ----------

   procedure Free
     (P   : in     ORB_Ctrl_Policy;
      Ptr : in out Policy_Access)
   is
   begin
      Free (ORB_Ctrl_Policy_Access (Ptr));
   end Free;

end Droopi.POA_Policies.Thread_Policy.ORB_Ctrl;
