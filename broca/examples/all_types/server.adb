with All_types.MyImpl;
with GenericServer; use GenericServer;

procedure server is
begin
   
   Repository_Id := new String'("IDL:all_types:1.0");
   My_Obj := new All_Types.MyImpl.Object;
   GenericServer.Main(My_Obj);

end Server;
