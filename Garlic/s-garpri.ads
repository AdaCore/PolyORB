--
--  $Id$
--

package System.Garlic.Priorities is

   --  This package defines priority constants which will be used
   --  throughout the whole PCS. If you change them and experiment better
   --  results (and can explain why), please report it to the maintenance
   --  team which will adjust future distributions.
   --  All the priorities are defined as if the application's main program
   --  was using System.Default_Priority as the base priority.

   Master_Termination_Priority : constant Priority := Priority'First;
   --  The main termination algorithm needs to run only when no other
   --  task is running and so runs at the lowest priority.

   RPC_Priority : constant Priority := Priority'Last;
   --  The RPC may be considered as an internal mechanism which will take
   --  place as soon as possible. However, since there is a good chance
   --  RPCs are slowed down because of communication speed, this shouldn't
   --  take too much time.

   Polling_Priority : constant Priority := RPC_Priority;
   --  When polling is being used, it needs to be executed as a high
   --  priority.

end System.Garlic.Priorities;
