with all_functions.Impl;

with CORBA;
with CORBA.Object;

with PolyORB.CORBA_P.Server_Tools; use PolyORB.CORBA_P.Server_Tools;
pragma Elaborate (PolyORB.CORBA_P.Server_Tools);

with PolyORB.Setup.No_Tasking_Server;
pragma Elaborate_All (PolyORB.Setup.No_Tasking_Server);
pragma Warnings (Off, PolyORB.Setup.No_Tasking_Server);

with Ada.Text_IO;

procedure Server is
   Ref : CORBA.Object.Ref;

begin
   Initiate_Servant (new all_functions.Impl.Object, Ref);
   Ada.Text_IO.Put_Line
     ("'" & CORBA.To_Standard_String (CORBA.Object.Object_To_String (Ref)) &
      "'");
   Initiate_Server;
end Server;

