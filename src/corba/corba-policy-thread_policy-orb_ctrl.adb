package body CORBA.Policy.Thread_Policy.Orb_Ctrl is

   use CORBA.Policy_Values;

   ------------
   -- Create --
   ------------

   function Create return Orb_Ctrl_Policy_Access
   is
      Policy : Orb_Ctrl_Policy_Access;
   begin
      Policy := new Orb_Ctrl_Policy'(Policy_Type =>
                                       THREAD_POLICY_ID,
                                     Value =>
                                       CORBA.Policy_Values.ORB_CTRL_MODEL);
      return Policy;
   end Create;

   -------------------------
   -- Check_Compatibility --
   -------------------------

   procedure Check_Compatibility (Self : Orb_Ctrl_Policy;
                                  OA   : CORBA.POA_Types.Obj_Adapter_Access)
   is
   begin
      null;
   end Check_Compatibility;

end CORBA.Policy.Thread_Policy.Orb_Ctrl;
