with Server;
with Do_Nothing;

pragma Warnings (Off, Server);

with PolyORB.ORB;
with PolyORB.Setup;
with PolyORB.Initialization;
pragma Warnings (Off);
with PolyORB.Setup.Thread_Pool_Server;
with PolyORB.POA_Config.RACWs;
pragma Warnings (On);

procedure Serverp is
begin
   Do_Nothing;
   PolyORB.Initialization.Initialize_World;
   PolyORB.ORB.Run (PolyORB.Setup.The_ORB, May_Poll => True);
end Serverp;
