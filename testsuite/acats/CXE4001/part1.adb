with PolyORB.Initialization;
pragma Warnings (Off);
with PolyORB.POA_Config.RACWs;
with PolyORB.Setup.Thread_Pool_Server;
pragma Warnings (On);

with CXE4001_A;

procedure Part1 is
begin
   PolyORB.Initialization.Initialize_World;
   CXE4001_A;
end Part1;
