package body CORBA.Policy.Servant_Retention_Policy.Retain is

   use CORBA.Policy_Values;

   ------------
   -- Create --
   ------------

   function Create return Retain_Policy_Access
   is
      Policy : Retain_Policy_Access;
   begin
      Policy := new Retain_Policy'(Policy_Type =>
                                     SERVANT_RETENTION_POLICY_ID,
                                   Value =>
                                     CORBA.Policy_Values.RETAIN);
      return Policy;
   end Create;

   -------------------------
   -- Check_Compatibility --
   -------------------------

   procedure Check_Compatibility (Self : Retain_Policy;
                                  OA   : CORBA.POA_Types.Obj_Adapter_Access)
   is
   begin
      null;
   end Check_Compatibility;

   ----------
   -- Free --
   ----------

   procedure Free (P   : in     Retain_Policy;
                   Ptr : in out Policy_Access)
   is
   begin
      Free (Retain_Policy_Access (Ptr));
   end Free;

end CORBA.Policy.Servant_Retention_Policy.Retain;

