pragma Warnings (Off);
with RCI;
with PolyORB.Setup.Thread_Pool_Server;
with PolyORB.POA_Config.RACWs;
with PolyORB.DSA_P.Partitions;
--  XXX This is the 'partition server': instantiate the partitions RCI.
pragma Warnings (On);

with PolyORB.Initialization;
with PolyORB.ORB;
with PolyORB.Setup;

procedure Server is
begin
   PolyORB.Initialization.Initialize_World;
   PolyORB.ORB.Run (PolyORB.Setup.The_ORB, May_Poll => True);
   --  No server main procedure.
end Server;
