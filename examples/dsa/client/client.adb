with Ada.Text_IO;
with RCI;
pragma Warnings (Off);
with PolyORB.Initialization;
with PolyORB.ORB.No_Tasking;
with PolyORB.No_Tasking;

with PolyORB.ORB;
with PolyORB.Setup;
with PolyORB.Setup.CORBA_Client;
pragma Warnings (On);

procedure Client is
   S : constant String := "Hello DSA world!";
begin
   Ada.Text_IO.Put_Line ("I said: " & S);
   Ada.Text_IO.Put_Line ("The server replied: "
     & RCI.echoString (S));
end Client;
