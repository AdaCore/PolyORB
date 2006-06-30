with PolyORB.DSA_P.Partitions;
with PolyORB.Log;
with PolyORB.Termination_Manager.Bootstrap;

package body System.DSA_Services is
   use PolyORB.Log;

   package L is new PolyORB.Log.Facility_Log ("system.dsa_services");
   procedure O (Message : String; Level : Log_Level := Debug)
     renames L.Output;
   function C (Level : Log_Level := Debug) return Boolean
     renames L.Enabled;
   pragma Unreferenced (C); --  For conditional pragma Debug

   use PolyORB.DSA_P.Partitions;
   use System.Partition_Interface;

begin
   --  Initialize the termination manager

   PolyORB.Termination_Manager.Bootstrap.Initialize_Termination_Manager;

   --  Allocate to this partition a local partition ID

   Set_Local_Partition_ID (RPC.Partition_ID (Allocate_Partition_ID ("")));

   pragma Debug (O ("DSA_Services Initialized"));
end System.DSA_Services;
