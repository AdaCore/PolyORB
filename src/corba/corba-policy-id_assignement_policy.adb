with CORBA.Policy.Id_Assignement_Policy.System;

package body CORBA.Policy.Id_Assignement_Policy is

   ------------
   -- Create --
   ------------

   function Create (Value : IdAssignementPolicyValue)
                   return IdAssignementPolicy_Access
   is
   begin
      case Value is
         when SYSTEM_ID =>
            return IdAssignementPolicy_Access
              (CORBA.Policy.Id_Assignement_Policy.System.Create);
         when others =>
            null;
            --  ??? Error! Raise exception?
      end case;
      return null;
   end Create;

   ------------
   -- Create --
   ------------

   function Create (P : IdAssignementPolicy)
                   return IdAssignementPolicy_Access
   is
   begin
      return Create (P.Value);
   end Create;

end CORBA.Policy.Id_Assignement_Policy;
