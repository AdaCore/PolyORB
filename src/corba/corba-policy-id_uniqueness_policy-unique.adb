package body CORBA.Policy.Id_Uniqueness_Policy.Unique is

   use CORBA.Policy_Values;

   ------------
   -- Create --
   ------------

   function Create return Unique_Id_Policy_Access
   is
      Policy : Unique_Id_Policy_Access;
   begin
      Policy := new Unique_Id_Policy'(Policy_Type =>
                                        ID_UNIQUENESS_POLICY_ID,
                                      Value =>
                                        CORBA.Policy_Values.UNIQUE_ID);
      return Policy;
   end Create;

   -------------------------
   -- Check_Compatibility --
   -------------------------

   procedure Check_Compatibility (Self : Unique_Id_Policy;
                                  OA   : CORBA.POA_Types.Obj_Adapter_Access)
   is
   begin
      null;
   end Check_Compatibility;

   ----------
   -- Free --
   ----------

   procedure Free (P   : in     Unique_Id_Policy;
                   Ptr : in out Policy_Access)
   is
   begin
      Free (Unique_Id_Policy_Access (Ptr));
   end Free;

end CORBA.Policy.Id_Uniqueness_Policy.Unique;
