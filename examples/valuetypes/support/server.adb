with Vtsupport.Supportsaverage;
with Vtsupport.LongList;
with Vtsupport.LongList.Helper;
with Vtsupport.LongList.Value_Impl;

with Portableserver;

with CORBA;
with CORBA.Object;

with Broca.Server_Tools; use Broca.Server_Tools;
pragma Elaborate (Broca.Server_Tools);

with Ada.Text_IO;

procedure Server is
   Ref : CORBA.Object.Ref;
   Vobj : Vtsupport.LongList.Value_Impl.Object_Ptr
     := Vtsupport.LongList.Value_Impl.Init;
   Serv : Vtsupport.LongList.Helper.Servant_Ref;
begin
   Vtsupport.LongList.Value_Impl.AddNumber (Vobj, CORBA.Long (1));
   Vtsupport.LongList.Value_Impl.AddNumber (Vobj, CORBA.Long (2));
   Vtsupport.LongList.Value_Impl.AddNumber (Vobj, CORBA.Long (3));

   Serv := Vtsupport.LongList.Helper.To_Servant
     (Vobj);
   
   Initiate_Servant (Portableserver.Servant (Serv), Ref);
   
   Ada.Text_IO.Put_Line
     ("'" & CORBA.To_Standard_String (CORBA.Object.Object_To_String (Ref)) &
      "'");
   
   Initiate_Server;
end Server;
