with CORBA.Policy.Implicit_Activation_Policy.No_Activation;

package body CORBA.Policy.Implicit_Activation_Policy is

   ------------
   -- Create --
   ------------

   function Create (Value : ImplicitActivationPolicyValue)
                   return ImplicitActivationPolicy_Access
   is
   begin
      case Value is
         when NO_IMPLICIT_ACTIVATION =>
            return ImplicitActivationPolicy_Access
              (CORBA.Policy.Implicit_Activation_Policy.No_Activation.Create);
         when others =>
            null;
            --  ??? Error! Raise exception?
      end case;
      return null;
   end Create;

   ------------
   -- Create --
   ------------

   function Create (P : ImplicitActivationPolicy)
                   return ImplicitActivationPolicy_Access
   is
   begin
      return Create (P.Value);
   end Create;

end CORBA.Policy.Implicit_Activation_Policy;
