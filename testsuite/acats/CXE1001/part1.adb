pragma Warnings (Off);
with PolyORB.Setup.Thread_Pool_Server;
with PolyORB.POA_Config.RACWs;
pragma Warnings (On);
with PolyORB.Initialization;

with CXE1001_A;

procedure Part1 is
begin
   PolyORB.Initialization.Initialize_World;
   CXE1001_A;
end Part1;
