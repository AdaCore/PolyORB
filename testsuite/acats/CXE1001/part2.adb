with System.PolyORB_Interface;
with PolyORB.Initialization;
with CXE1001_P;
with CXE1001_Q;
with Report;
with CXE1001_B;
with PolyORB.Setup.Client;
with PolyORB.POA_Config.RACWs;

procedure Part2 is
begin
   PolyORB.Initialization.Initialize_World;
   CXE1001_B;
end Part2;
