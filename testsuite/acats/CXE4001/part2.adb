with PolyORB.Initialization;
pragma Warnings (Off);
with PolyORB.POA_Config.RACWs;
with PolyORB.Setup.Thread_Pool_Server;
pragma Warnings (On);

with CXE4001_B;

procedure Part2 is
begin
   PolyORB.Initialization.Initialize_World;
   CXE4001_B;
end Part2;
