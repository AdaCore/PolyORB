--  Global POA configuration.

--  $Id$

package body Droopi.POA_Config is

   The_Configuration : Configuration_Access;

   procedure Set_Configuration
     (C : Configuration_Access) is
   begin
      pragma Assert (The_Configuration = null and then C /= null);
      The_Configuration := C;
   end Set_Configuration;

   function Configuration return Configuration_Access is
   begin
      pragma Assert (The_Configuration /= null);
      return The_Configuration;
   end Configuration;

end Droopi.POA_Config;

