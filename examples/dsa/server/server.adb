pragma Warnings (Off);
with RCI;
with PolyORB.Initialization;
--  with PolyORB.ORB.Thread_Pool;
--  with PolyORB.Protected_Objects;
with PolyORB.ORB.No_Tasking;
with PolyORB.No_Tasking;

with PolyORB.ORB;
with PolyORB.Setup;
with PolyORB.Setup.Server;
pragma Warnings (On);

procedure Server is
begin
   PolyORB.Initialization.Initialize_World;
   PolyORB.ORB.Run (PolyORB.Setup.The_ORB, May_Poll => True);
end Server;
