with PolyORB.Initialization;
with PolyORB.POA_Config.RACWs;
with PolyORB.Setup.Thread_Pool_Server;

with CXE4006_Part_B;
with CXE4006_B;

procedure Part2 is
begin
   PolyORB.Initialization.Initialize_World;
   CXE4006_B;
end Part2;
