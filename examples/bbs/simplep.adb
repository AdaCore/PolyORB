with PolyORB.Initialization;
with PolyORB.Setup.Client;
pragma Warnings (Off, PolyORB.Setup.Client);
with Simple;

procedure SimpleP is
begin
   PolyORB.Initialization.Initialize_World;
   Simple;
end SimpleP;
