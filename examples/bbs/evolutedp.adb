with PolyORB.Initialization;
with PolyORB.Setup.Thread_Pool_Server;
--  We have RACWs: better initialize some TSAPs!
--  We also have a main loop, so we need parallel processing:
--  use a thread pool.
pragma Warnings (Off, PolyORB.Setup.Thread_Pool_Server);
with PolyORB.POA_Config.RACWs;
pragma Warnings (Off, PolyORB.POA_Config.RACWs);
with Evoluted;

procedure EvolutedP is
begin
   PolyORB.Initialization.Initialize_World;
   Evoluted;
end EvolutedP;
