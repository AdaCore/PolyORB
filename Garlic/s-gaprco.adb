with System.Garlic.Loopback;
pragma Elaborate_All (System.Garlic.Loopback);

with System.Garlic.TCP;
pragma Elaborate_All (System.Garlic.TCP);

package body System.Garlic.Protocols.Config is

   --  This package should be created during GARLIC installation.
   --  It should register all the protocols present in the distribution

   procedure Register (P : in Protocol_Access);
   --  Register the protocol as a present protocol.

   --------------
   -- Register --
   --------------

   procedure Register (P : in Protocol_Access) is
   begin
      for I in Protocol_Table'Range loop
         if Protocol_Table (I) = null then
            Protocol_Table (I) := P;
            return;
         end if;
      end loop;
      raise Constraint_Error;
   end Register;

   ------------
   -- Create --
   ------------

   procedure Create is
   begin
      Register (System.Garlic.Loopback.Create);
      Register (System.Garlic.TCP.Create);
   end Create;

end System.Garlic.Protocols.Config;
