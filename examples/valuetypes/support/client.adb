with Ada.Command_Line;
with Ada.Text_Io; use Ada.Text_Io;
with CORBA; use CORBA;
with CORBA.ORB;

with Broca.Server_Tools; use Broca.Server_Tools;
pragma Elaborate (Broca.Server_Tools);

with Vtsupport.Supportsaverage;

procedure Client is

   Iref : Vtsupport.Supportsaverage.Ref;
   Avg : CORBA.Double;
begin
   CORBA.ORB.String_To_Object
     (CORBA.To_CORBA_String (Ada.Command_Line.Argument (1)),
      IRef);
   if Vtsupport.Supportsaverage.Is_Nil (Iref) then
      Put_Line ("main : cannot invoke on a nil reference");
      return;
   end if;

   Put_Line ("Sending request");
   Avg := Vtsupport.Supportsaverage.Get_Average (Iref);
   Put_Line ("Average = " & CORBA.Double'Image (Avg));

   Put_Line ("Client finished");
   
end Client;
