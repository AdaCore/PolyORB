with PolyORB.Initialization;
with PolyORB.POA_Config.RACWs;
with PolyORB.Setup.Thread_Pool_Server;

with CXE2001_B;
with CXE2001_Part_B;

procedure Part2 is
begin
   PolyORB.Initialization.Initialize_World;
   CXE2001_B;
end Part2;
