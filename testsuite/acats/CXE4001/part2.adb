with PolyORB.Initialization;
with PolyORB.POA_Config.RACWs;
with PolyORB.Setup.Thread_Pool_Server;

with CXE4001_Partition_B;
with CXE4001_B;

procedure Part2 is
begin
   PolyORB.Initialization.Initialize_World;
   CXE4001_B;
end Part2;
