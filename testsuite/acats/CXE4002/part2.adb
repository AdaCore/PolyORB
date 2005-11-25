with PolyORB.Initialization;
pragma Warnings (Off);
with PolyORB.POA_Config.RACWs;
with PolyORB.Setup.Thread_Pool_Server;
pragma Warnings (On);

with CXE4002_B;

procedure Part2 is
begin
   PolyORB.Initialization.Initialize_World;
   CXE4002_B;
end Part2;
