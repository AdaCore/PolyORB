with Server;
with Do_Nothing;
pragma Warnings (Off, Server);

with PolyORB.If_Descriptors;
with PolyORB.If_Descriptors.CORBA_IR;
with PolyORB.POA_Config.Proxies;
pragma Warnings (Off, PolyORB.POA_Config.Proxies);

with PolyORB.ORB;
with PolyORB.Setup;
with PolyORB.Initialization;

pragma Warnings (Off);
with PolyORB.Setup.Thread_Pool_Server;
with PolyORB.POA_Config.RACWs;
pragma Warnings (On);

procedure Ir_Serverp is
begin
   Do_Nothing;
   PolyORB.Initialization.Initialize_World;
   PolyORB.If_Descriptors.Default_If_Descriptor
     := new PolyORB.If_Descriptors.CORBA_IR.IR_If_Descriptor;
   PolyORB.ORB.Run (PolyORB.Setup.The_ORB, May_Poll => True);
end Ir_Serverp;
