with PolyORB.Initialization;
with PolyORB.Setup.Client;
pragma Warnings (Off, PolyORB.Setup.Client);
with Evoluted;

procedure EvolutedP is
begin
   PolyORB.Initialization.Initialize_World;
   Evoluted;
end EvolutedP;
