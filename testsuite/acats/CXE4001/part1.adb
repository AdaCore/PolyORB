with PolyORB.Initialization;
with PolyORB.POA_Config.RACWs;
with PolyORB.Setup.Thread_Pool_Server;

with CXE4001_Partition_A;
with CXE4001_A;

procedure Part1 is
begin
   PolyORB.Initialization.Initialize_World;
   CXE4001_A;
end Part1;
