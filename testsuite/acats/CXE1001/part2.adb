with PolyORB.Initialization;
with PolyORB.Setup.Client;
with PolyORB.POA_Config.RACWs;

with CXE1001_B;

procedure Part2 is
begin
   PolyORB.Initialization.Initialize_World;
   CXE1001_B;
end Part2;
