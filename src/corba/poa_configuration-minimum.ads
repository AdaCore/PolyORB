package POA_Configuration.Minimum is

   type Minimum_Configuration is new Configuration with null record;

   procedure Initialize
     (C : Minimum_Configuration;
      F : Droopi.POA_Policies.Policies_Factory);

end POA_Configuration.Minimum;
