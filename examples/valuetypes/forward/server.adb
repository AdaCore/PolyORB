with Tvtforward.Main.Impl;
with CORBA;
with CORBA.Object;

with Broca.Server_Tools; use Broca.Server_Tools;
pragma Elaborate (Broca.Server_Tools);

with Ada.Text_IO;

procedure Server is
   Ref : CORBA.Object.Ref;

begin
   Initiate_Servant (new Tvtforward.Main.Impl.Object, Ref);
   Ada.Text_IO.Put_Line
     ("'" & CORBA.To_Standard_String (CORBA.Object.Object_To_String (Ref)) &
      "'");
   Initiate_Server;
end Server;
