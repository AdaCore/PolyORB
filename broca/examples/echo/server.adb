with Echo.My_Impl;
with GenericServer; use GenericServer;

procedure Server is
begin
   
   My_Obj := new Echo.My_Impl.Object;
   GenericServer.Main(My_Obj);
   
end Server;
