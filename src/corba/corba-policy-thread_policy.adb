with CORBA.Policy.Thread_Policy.Orb_Ctrl;

package body CORBA.Policy.Thread_Policy is

   ------------
   -- Create --
   ------------

   function Create (Value : ThreadPolicyValue)
                   return ThreadPolicy_Access
   is
   begin
      case Value is
         when ORB_CTRL_MODEL =>
            return ThreadPolicy_Access
              (CORBA.Policy.Thread_Policy.Orb_Ctrl.Create);
         when others =>
            null;
            --  ??? Error! Raise exception?
      end case;
      return null;
   end Create;

   ------------
   -- Create --
   ------------

   function Create (P : ThreadPolicy)
                   return ThreadPolicy_Access
   is
   begin
      return Create (P.Value);
   end Create;

end CORBA.Policy.Thread_Policy;
