--  Global POA configuration.

--  $Id$

with Droopi.POA_Policies;

package POA_Configuration is

   --  pragma Preelaborate;

   type Configuration_Type is abstract tagged null record;
   type Configuration_Access is access all Configuration_Type'Class;

   procedure Initialize
     (C : Configuration_Type)
      is abstract;
   --  Create all policies available in this configuration,
   --  and register them with policy repository F.

   function Default_Policies
     (C : Configuration_Type)
     return Droopi.POA_Policies.PolicyList_Access
      is abstract;
   --  Return the list of default OA policies for this configuration.

   procedure Set_Configuration
     (C : Configuration_Access);
   --  Set the configuration for the whole runtime.
   --  May be called only once. C must be non-null.

   function Configuration return Configuration_Access;
   --  The value set by Set_Configuration.

private

   pragma Inline (Set_Configuration);
   pragma Inline (Configuration);

end POA_Configuration;
