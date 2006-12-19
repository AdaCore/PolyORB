with PolyORB.Initialization;
with PolyORB.POA_Config.RACWs;
with PolyORB.Setup.Thread_Pool_Server;

with CXE4005_Part_B;
with CXE4005_B;

procedure Part2 is
begin
   PolyORB.Initialization.Initialize_World;
   CXE4005_B;
end Part2;
