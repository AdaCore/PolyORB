with PolyORB.Initialization;
with PolyORB.POA_Config.RACWs;
with PolyORB.Setup.Thread_Pool_Server;

with CXE4002_B;

procedure Part2 is
begin
   PolyORB.Initialization.Initialize_World;
   CXE4002_B;
end Part2;
