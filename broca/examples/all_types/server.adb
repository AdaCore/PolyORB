with All_types.MyImpl;
with GenericServer; use GenericServer;

procedure server is
begin
   
   My_Obj := new All_Types.MyImpl.Object;
   GenericServer.Main(My_Obj);

end Server;
