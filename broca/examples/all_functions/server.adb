with All_functions.Impl;

with CORBA;
with CORBA.Object;

with Broca.Basic_Startup; use Broca.Basic_Startup;
pragma Elaborate (Broca.Basic_Startup);

with Ada.Text_IO;

procedure Server is
   Ref : CORBA.Object.Ref;

begin
   Initiate_Servant (new All_functions.Impl.Object, Ref);
   Ada.Text_IO.Put_Line
     ("'" & CORBA.To_Standard_String (CORBA.Object.Object_To_String (Ref)) &
      "'");
   Initiate_Server;
end Server;

