with M1.Echo.Impl;
with GenericServer; use GenericServer;

procedure Server is
begin
   
   Repository_Id := new String'("IDL:M1.Echo:1.0");
   My_Obj := new M1.Echo.Impl.Object;
   GenericServer.Main(My_Obj);
   
end Server;
