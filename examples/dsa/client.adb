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
   RAS : RCI.echo_RAS;

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
         Put_Line ("Got not null: " & RT.Tekitoa (Obj.all) & " is alive!");
      end if;
   end Try_RACW;

   Z : constant RCI.Complex := (Re => 2.0, Im => 3.0);

begin
   Put_Line ("I said: " & S);
   Put_Line ("The server replied: "
     & RCI.echoString (S));
   RAS := RCI.echoString'Access;
   Put_Line ("through RAS: " & RAS (S & " (RASI)"));
   Put_Line ("through RAS: " & RAS.all (S & " (RASE)"));

   Try_RACW ("");
   Try_RACW ("Elvis");

   Put_Line ("|2 + 3i|^2 = " & Float'Image (RCI.Modulus2 (Z)));
end Client;
