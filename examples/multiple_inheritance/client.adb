----                                                                    ----
----     This in an example which is hand-written                       ----
----                                                                    ----
----             procedure client                                       ----
----                                                                    ----
----     author : Sebastien Ponce, Fabien Azavant                       ----
----                                                                    ----
----------------------------------------------------------------------------

with Ada.Command_Line ;
with Corba.Object ; use Corba.Object ;
with Corba, Corba.Orb, Corba.Boa, Corba.Object ;
with Weapon ;
with Vehicle ;
with Tank ;

with Text_IO ; use Text_IO ;


procedure Client is

   -- initialization of the ORB
   Orb : Corba.Orb.Object := Corba.Orb.Orb_Init("omniORB2");

   O : Corba.Object.Ref;
   T : Tank.Ref ;
   W : Weapon.Ref;
   V : Vehicle.Ref;
   Distance : Weapon.Dist;
   L1 : Weapon.Longueur_Array;

   Ior : Corba.String ;
begin

   if Ada.Command_Line.Argument_Count < 1 then
      Put_Line ("usage : client <IOR_string_from_server>") ;
      return ;
   end if ;

   -- transforms the Ada string into Corba.String
   IOR := Corba.To_Corba_String(Ada.Command_Line.Argument(1)) ;
   Corba.Orb.String_To_Object(IOR, T) ;

   L1 := (10, 15, 12);
   Distance := (Longueur => L1 , Largeur => 13);
   Put_Line("");
   Put_Line("");
   Put_Line("Test the attribute value with simpl inheritance");
   Put_Line("The value of Mark is set to : AdaBroker");
   Tank.Set_Mark(T, Corba.To_Corba_String(Standard.String'("AdaBroker")));

   Put_Line("");
   Put_Line("Check of the attribute:");
   Put_Line ("   Mark value is : "
             & Corba.To_standard_String(Tank.Get_Mark(T)));

   Put_Line("");
   Put_Line("Check of the value of parent's Mark:"
            & Corba.To_standard_String(Vehicle.Get_Mark(Vehicle.To_Ref(T))));
   Put_Line("");
   Put_Line("");
   Put_Line("Test of methods with simple inheritance :");
   Put_Line("I can drive : "
            &  Corba.Boolean'Image(Tank.Can_drive(T, 18)));
   Put_Line("");
   Put_Line("");
   Put_Line("Test of methods with multiple inheritance :");
   Tank.Shoot(T, Distance) ;
   Put_Line("Tank is down ....., it works");

   Put_Line("");
   Put_Line("");
   Put_Line("Test of methods with multiple inheritance and cast :");
   W := Weapon.To_Ref(T);
   Weapon.Shoot(W, Distance);
   Put_Line("Weapon is down ..... it works");
   Put_Line("");
   Put_Line("Test of cast from Weapon to Vehicle:");
   V := Vehicle.To_Ref(W);
   Put_Line("  the Mark is :" & Corba.To_standard_String(Vehicle.Get_Mark(V)));
   Put_Line("Cast works");
   Put_Line("");
   Put_Line("Cast the former to Corba.Object.Ref  :");
   O := CORBA.Object.Ref(V);
   Put_Line("Final Cast from Corba.Object.Ref to Tank:");
   T := Tank.To_Ref(O);
   Put_Line("  Mark is :" & Corba.To_standard_String(Tank.Get_Mark(T)));
   Put_Line("It works");


   Put_Line("");
   Put_Line("Test completed!");


end Client ;






