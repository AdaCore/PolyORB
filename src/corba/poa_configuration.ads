with CORBA.Policy;

package POA_Configuration is

   type Configuration is abstract tagged null record;

   procedure Initialize (C : Configuration;
                         F : CORBA.Policy.Policies_Factory)
      is abstract;

end POA_Configuration;

