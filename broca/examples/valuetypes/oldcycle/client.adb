with Ada.Command_Line;
with Ada.Text_Io; use Ada.Text_Io;
with CORBA;
with CORBA.ORB;

with Broca.Server_Tools; use Broca.Server_Tools;
pragma Elaborate (Broca.Server_Tools);

with Cycle.Node;
with Cycle.NodeManipulator;

procedure Client is

   NMRef : Cycle.NodeManipulator.Ref;
   Noderef : Cycle.Node.Value_Ref
     := Cycle.Node.Value_Ref (Cycle.Node.CreateElement (0));
   
begin
   CORBA.ORB.String_To_Object
     (CORBA.To_CORBA_String (Ada.Command_Line.Argument (1)),
      NMRef);
   if Cycle.NodeManipulator.Is_Nil (NMRef) then
      Put_Line ("main : cannot invoke on a nil reference");
      return;
   end if;
   
   while True loop
      Noderef := Cycle.Node.CmdLineManipulate (Noderef);
      Put_Line ("Sending:");
      Cycle.Node.Print (Noderef);
      Cycle.NodeManipulator.remoteManipulate (NMRef, Noderef);
      Put_Line("");
      Put_Line ("Received:");
      Cycle.Node.Print (Noderef);
   end loop;

end Client;
