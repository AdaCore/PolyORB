with CORBA.Policy_Types;

package body CORBA.Policy.Thread_Policy.Orb_Ctrl is

   use CORBA.Policy_Values;
   use CORBA.Policy_Types;

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
                                  OA   : Droopi.POA_Types.Obj_Adapter_Access)
   is
   begin
      null;
   end Check_Compatibility;

   ----------
   -- Free --
   ----------

   procedure Free (P   : in     Orb_Ctrl_Policy;
                   Ptr : in out Policy_Access)
   is
   begin
      Free (Orb_Ctrl_Policy_Access (Ptr));
   end Free;

end CORBA.Policy.Thread_Policy.Orb_Ctrl;
