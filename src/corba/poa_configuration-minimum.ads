package POA_Configuration.Minimum is

   type Minimum_Configuration is new Configuration with null record;

   procedure Initialize
     (C : Minimum_Configuration;
      F : Droopi.POA_Policies.Policy_Repository);

   function Default_Policies
     (C : Minimum_Configuration)
     return Droopi.POA_Policies.PolicyList_Access;

end POA_Configuration.Minimum;
