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

end CORBA.Policy.Servant_Retention_Policy.Retain;

