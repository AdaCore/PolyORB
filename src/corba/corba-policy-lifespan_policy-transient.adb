package body CORBA.Policy.Lifespan_Policy.Transient is

   use CORBA.Policy_Values;

   ------------
   -- Create --
   ------------

   function Create return Transient_Policy_Access
   is
      Policy : Transient_Policy_Access;
   begin
      Policy := new Transient_Policy'(Policy_Type =>
                                        LIFESPAN_POLICY_ID,
                                      Value =>
                                        CORBA.Policy_Values.TRANSIENT);
      return Policy;
   end Create;

   -------------------------
   -- Check_Compatibility --
   -------------------------

   procedure Check_Compatibility (Self : Transient_Policy;
                                  OA   : CORBA.POA_Types.Obj_Adapter_Access)
   is
   begin
      null;
   end Check_Compatibility;

   ----------
   -- Free --
   ----------

   procedure Free (P   : in     Transient_Policy;
                   Ptr : in out Policy_Access)
   is
   begin
      Free (Transient_Policy_Access (Ptr));
   end Free;

end CORBA.Policy.Lifespan_Policy.Transient;
