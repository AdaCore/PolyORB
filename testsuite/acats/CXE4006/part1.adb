with PolyORB.Initialization;
with PolyORB.POA_Config.RACWs;
with PolyORB.Setup.Thread_Pool_Server;

--  with PolyORB.DSA_P.Partitions;
with CXE4006_Part_A1;
with CXE4006_Part_A2;
with CXE4006_A;

procedure Part1 is
begin
   PolyORB.Initialization.Initialize_World;
   CXE4006_A;
end Part1;
