pragma Warnings (Off);
with RCI;
with PolyORB.Initialization;

with PolyORB.ORB;
with PolyORB.Setup;
with PolyORB.Setup.Thread_Pool_Server;
with PolyORB.POA_Config.RACWs;
pragma Warnings (On);

procedure Server is
begin
   PolyORB.ORB.Run (PolyORB.Setup.The_ORB, May_Poll => True);
end Server;
