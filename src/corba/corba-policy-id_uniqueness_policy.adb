with CORBA.Policy.Id_Uniqueness_Policy.Unique;

package body CORBA.Policy.Id_Uniqueness_Policy is

   ------------
   -- Create --
   ------------

   function Create (Value : IdUniquenessPolicyValue)
                   return IdUniquenessPolicy_Access
   is
   begin
      case Value is
         when UNIQUE_ID =>
            return IdUniquenessPolicy_Access
              (Unique.Create);
         when others =>
            null;
            --  ??? Error! Raise exception?
      end case;
      return null;
   end Create;

   ------------
   -- Create --
   ------------

   function Create (P : IdUniquenessPolicy)
                   return IdUniquenessPolicy_Access
   is
   begin
      return Create (P.Value);
   end Create;


end CORBA.Policy.Id_Uniqueness_Policy;
