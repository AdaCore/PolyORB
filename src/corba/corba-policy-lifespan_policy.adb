with CORBA.Policy.Lifespan_Policy.Transient;
with CORBA.Policy_Values;

package body CORBA.Policy.Lifespan_Policy is

   ------------
   -- Create --
   ------------

   function Create (Value : LifespanPolicyValue)
                   return LifespanPolicy_Access
   is

   begin
      case Value is
         when CORBA.Policy_Values.TRANSIENT =>
            return LifespanPolicy_Access
              (CORBA.Policy.Lifespan_Policy.Transient.Create);
         when others =>
            null;
            --  ??? Error! Raise exception?
      end case;
      return null;
   end Create;

   ------------
   -- Create --
   ------------

   function Create (P : LifespanPolicy)
                   return LifespanPolicy_Access
   is
   begin
      return Create (P.Value);
   end Create;

end CORBA.Policy.Lifespan_Policy;
