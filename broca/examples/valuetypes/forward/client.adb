with Ada.Command_Line;
with Ada.Text_Io; use Ada.Text_Io;
with CORBA; use CORBA;
with CORBA.ORB;

with Broca.Server_Tools; use Broca.Server_Tools;
pragma Elaborate (Broca.Server_Tools);

with Tvtforward.First;
with Tvtforward.Second;
with Tvtforward.Main;

procedure Client is

   Fref : Tvtforward.First.Value_Ref;
   Sref : Tvtforward.Second.Value_Ref;
   Mref : Tvtforward.Main.Ref;
   
begin
   CORBA.ORB.String_To_Object
     (CORBA.To_CORBA_String (Ada.Command_Line.Argument (1)),
      MRef);
   if Tvtforward.Main.Is_Nil (Mref) then
      Put_Line ("main : cannot invoke on a nil reference");
      return;
   end if;

   Fref := Tvtforward.First.Value_Ref
     (Tvtforward.First.Create (123, To_CORBA_String ("hello world")));

   Put_Line ("Param :");
   Tvtforward.First.Print (Fref);

   Put_Line ("Sending request");
   Sref := Tvtforward.Main.SendSecond (Mref, Fref);

   Put_Line ("received :");
   Tvtforward.First.Print
     (Tvtforward.First.Convert_Forward.From_Forward
      (Tvtforward.Second.Get_Att (Sref)));

   Put_Line ("Client finished");
   
end Client;
