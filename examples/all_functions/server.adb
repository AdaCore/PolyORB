with All_functions.Impl;
with GenericServer; use GenericServer;

procedure Server is
begin

   Repository_Id := new String'("IDL:all_types:1.0");
   My_Obj := new All_Functions.Impl.Object;
   GenericServer.Main(My_Obj);

end Server;
