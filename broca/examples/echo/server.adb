with Echo.Impl;
with GenericServer; use GenericServer;

procedure Server is
begin
   
   Repository_Id := new String'("IDL:Echo:1.0");
   My_Obj := new Echo.Impl.Object;
   GenericServer.Main(My_Obj);
   
end Server;
