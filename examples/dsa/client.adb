with Ada.Text_IO; use Ada.Text_IO;
with RCI;
with RT;

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

   procedure Try_RACW (Name : String);
   procedure Try_RACW (Name : String) is
      use type RT.RACW;
      Obj : RT.RACW;
   begin
      Put_Line ("Trying RACW with Name = """ & Name & """");
      Obj := RCI.Get_Obj (Name);
      if Obj = null then
         Put_Line ("Got null!");
      else
         Put_Line ("Got not null!");
      end if;
   end Try_RACW;

begin
   Put_Line ("I said: " & S);
   Put_Line ("The server replied: "
     & RCI.echoString (S));
   Try_RACW ("");
end Client;
