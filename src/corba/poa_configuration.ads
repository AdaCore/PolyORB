with Droopi.POA_Policies;

package POA_Configuration is

   type Configuration is abstract tagged null record;
   type Configuration_Access is access all Configuration'Class;

   procedure Initialize
     (C : Configuration;
      F : Droopi.POA_Policies.Policy_Repository)
      is abstract;
   --  Create all policies available in this configuration,
   --  and register them with policy repository F.

   function Default_Policies
     (C : Configuration)
     return Droopi.POA_Policies.PolicyList_Access
      is abstract;
   --  Return the list of default OA policies for this configuration.

end POA_Configuration;

