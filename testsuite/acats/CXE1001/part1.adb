with System.PolyORB_Interface;
with PolyORB.Setup.Thread_Pool_Server;
with PolyORB.POA_Config.RACWs;
with PolyORB.Initialization;
with CXE1001_P;
with CXE1001_Q;
with Report;
with CXE1001_A;

procedure Part1 is
begin
   PolyORB.Initialization.Initialize_World;
   CXE1001_A;
end Part1;
