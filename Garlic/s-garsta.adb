--
--  $Id$
--

--  Here, you must 'with' and 'pragma Elaborate_All' all the protocols you
--  want to use. Look for other sections called "Protocol section" below
--  to make sure you initialize the protocols you want to use.

with System.Garlic.TCP;
pragma Elaborate_All (System.Garlic.TCP);

with System.Garlic.Debug; use System.Garlic.Debug;
pragma Elaborate_All (System.Garlic.Debug);

with System.Garlic.Protocols; use System.Garlic.Protocols;
pragma Elaborate_All (System.Garlic.Protocols);

with System.Garlic.Options; use System.Garlic.Options;
pragma Elaborate_All (System.Garlic.Options);

with System.Garlic.Physical_Location; use System.Garlic.Physical_Location;
pragma Elaborate_All (System.Garlic.Physical_Location);

with System.Garlic.Heart; use System.Garlic.Heart;
pragma Elaborate_All (System.Garlic.Heart);

with System.Garlic.Remote; use System.Garlic.Remote;
pragma Elaborate_All (System.Garlic.Remote);

with System.Garlic.Termination; use System.Garlic.Termination;
pragma Elaborate_All (System.Garlic.Termination);

with System.RPC;

package body System.Garlic.Startup is

   Private_Debug_Key : constant Debug_Key :=
     Debug_Initialize ("STARTUP", "(s-garsta): ");
   procedure D
     (Level   : in Debug_Levels;
      Message : in String;
      Key     : in Debug_Key := Private_Debug_Key)
     renames Print_Debug_Info;

   use System.Garlic.Protocols;

   Max_Protocols : constant := 10;
   Protocols     : array (1 .. Max_Protocols) of Protocol_Access;

   procedure Register (P : in Protocol_Access);
   --  Register the protocol as a present protocol.

   --------------
   -- Register --
   --------------

   procedure Register (P : in Protocol_Access) is
   begin
      for I in Protocols'Range loop
         if Protocols (I) = null then
            Protocols (I) := P;
            return;
         end if;
      end loop;
      raise Constraint_Error;
   end Register;

begin

   D (D_Elaborate, "Entering partition startup phase");

   --  Phase (1) (see s-garlic.ads)

   if Get_Detach then
      D (D_Elaborate, "Detaching, you won't see any more debug messages");
      Detach;
   end if;

   --  Phase (2) (see s-garlic.ads)

   --  Add a line by protocol, such as: Register (<my_protocol>.Create);
   Register (System.Garlic.TCP.Create);

   declare
      Boot_Location : constant Location :=
        To_Location (Options.Get_Boot_Server);
      Boot_Protocol : constant Protocol_Access := Get_Protocol (Boot_Location);
      Boot_Data     : constant String := Get_Data (Boot_Location);
      Is_Master     : constant Boolean := not Get_Is_Slave;
      New_Location  : Location;
   begin

      if Boot_Protocol = null then
         D (D_Elaborate, "No boot protocol, aborting");
         raise System.RPC.Communication_Error;
      end if;

      --  Phase (3) (see s-garlic.ads)

      Is_Boot_Partition (Is_Master);

      --  Phase (4) (see s-garlic.ads)

      for I in Protocols'Range loop
         exit when Protocols (I) = null;
         if Protocols (I) = Boot_Protocol then
            Set_Boot_Data (Boot_Protocol, True, Boot_Data, Is_Master);
            Is_Boot_Partition (Is_Master);
            Set_My_Location (To_Location (Boot_Protocol,
                                          Get_Info (Boot_Protocol)));
            if Is_Master then
               Set_Boot_Location (To_Location (Boot_Protocol,
                                               Get_Info (Boot_Protocol)));
            else
               Set_Boot_Location (To_Location (Boot_Protocol, Boot_Data));
            end if;
         else
            Set_Boot_Data (Boot_Protocol);
         end if;
      end loop;

      --  Phase (5) (see s-garlic.ads)

      Termination.Initialize;

   end;

   --  Phase (6) (see s-garlic.ads)

   declare
      Partition : constant System.RPC.Partition_ID := Get_My_Partition_ID;
   begin
      null;
   end;

   D (D_Elaborate, "Startup phase terminated");

end System.Garlic.Startup;
