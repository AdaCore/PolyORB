pragma Warnings (Off);
with RCI;
with PolyORB.Setup.Thread_Pool_Server;
with PolyORB.POA_Config.RACWs;
pragma Warnings (On);

with PolyORB.Initialization;
with PolyORB.ORB;
with PolyORB.Setup;
with PolyORB.DSA_P.Partitions;
with System.RPC;

procedure Server is
   This_Partition_ID : System.RPC.Partition_ID;
begin
   PolyORB.Initialization.Initialize_World;
   This_Partition_ID := System.RPC.Partition_ID
     (PolyORB.DSA_P.Partitions.Allocate_Partition_ID ("serverp"));
   PolyORB.ORB.Run (PolyORB.Setup.The_ORB, May_Poll => True);
   --  No server main procedure.
end Server;
