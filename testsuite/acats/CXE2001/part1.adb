with PolyORB.Initialization;
with PolyORB.POA_Config.RACWs;
with PolyORB.Setup.Thread_Pool_Server;

pragma Warnings (Off, PolyORB.POA_Config.RACWs);
pragma Warnings (Off, PolyORB.Setup.Thread_Pool_Server);

with CXE2001_A;

procedure Part1 is
begin
   PolyORB.Initialization.Initialize_World;
   CXE2001_A;
end Part1;
