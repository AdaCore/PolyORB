with all_functions.Impl;

with CORBA;
with CORBA.Object;
with CORBA.ORB;

with PolyORB.CORBA_P.Server_Tools; use PolyORB.CORBA_P.Server_Tools;
pragma Elaborate (PolyORB.CORBA_P.Server_Tools);

with PolyORB.Setup.Thread_Pool_Server;
pragma Elaborate_All (PolyORB.Setup.Thread_Pool_Server);
pragma Warnings (Off, PolyORB.Setup.Thread_Pool_Server);

--  Note : the server must execute two tasks concurrently
--         to pass oneway tests

with Ada.Text_IO;

procedure Server is
   Ref : CORBA.Object.Ref;

begin
   CORBA.ORB.Initialize ("ORB");
   Initiate_Servant (new all_functions.Impl.Object, Ref);
   Ada.Text_IO.Put_Line
     ("'" & CORBA.To_Standard_String (CORBA.Object.Object_To_String (Ref)) &
      "'");
   Initiate_Server;
end Server;

