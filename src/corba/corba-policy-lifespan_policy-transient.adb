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

end CORBA.Policy.Lifespan_Policy.Transient;
