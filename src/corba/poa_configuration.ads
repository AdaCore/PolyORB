with Droopi.POA_Policies;

package POA_Configuration is

   type Configuration is abstract tagged null record;

   procedure Initialize
     (C : Configuration;
      F : Droopi.POA_Policies.Policies_Factory)
      is abstract;

end POA_Configuration;

