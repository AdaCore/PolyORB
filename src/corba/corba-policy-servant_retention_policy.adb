with CORBA.Policy.Servant_Retention_Policy.Retain;
with CORBA.Policy_Values;

package body CORBA.Policy.Servant_Retention_Policy is

   ------------
   -- Create --
   ------------

   function Create (Value : ServantRetentionPolicyValue)
                   return ServantRetentionPolicy_Access
   is
   begin
      case Value is
         when CORBA.Policy_Values.RETAIN =>
            return ServantRetentionPolicy_Access
              (CORBA.Policy.Servant_Retention_Policy.Retain.Create);
         when others =>
            null;
            --  ??? Error! Raise exception?
      end case;
      return null;
   end Create;

   ------------
   -- Create --
   ------------

   function Create (P : ServantRetentionPolicy)
     return ServantRetentionPolicy_Access
   is
   begin
      return Create (P.Value);
   end Create;

end CORBA.Policy.Servant_Retention_Policy;

