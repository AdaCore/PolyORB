with CORBA.Policy.Request_Processing_Policy.Active_Object_Map_Only;

package body CORBA.Policy.Request_Processing_Policy is

   ------------
   -- Create --
   ------------

   function Create (Value : RequestProcessingPolicyValue)
                   return RequestProcessingPolicy_Access
   is
   begin
      case Value is
         when USE_ACTIVE_OBJECT_MAP_ONLY =>
            return RequestProcessingPolicy_Access
              (CORBA.Policy.Request_Processing_Policy.
               Active_Object_Map_Only.Create);
         when others =>
            null;
            --  ??? Error! Raise exception?
      end case;
      return null;
   end Create;

   ------------
   -- Create --
   ------------

   function Create (P : RequestProcessingPolicy)
                   return RequestProcessingPolicy_Access
   is
   begin
      return Create (P.Value);
   end Create;

end CORBA.Policy.Request_Processing_Policy;
