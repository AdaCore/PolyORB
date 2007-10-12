with PolyORB.Initialization;
with PolyORB.POA_Config.RACWs;
with PolyORB.Setup.Thread_Pool_Server;

--  with PolyORB.DSA_P.Partitions;
with CXE4005_Part_A1;
with CXE4005_Part_A2;
with CXE4005_A;

procedure Part1 is
begin
   PolyORB.Initialization.Initialize_World;
   CXE4005_A;
end Part1;
