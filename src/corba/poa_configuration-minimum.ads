package POA_Configuration.Minimum is

   type Minimum_Configuration is new Configuration with null record;

   procedure Initialize (C : Minimum_Configuration;
                         F : CORBA.Policy.Policies_Factory);

end POA_Configuration.Minimum;
