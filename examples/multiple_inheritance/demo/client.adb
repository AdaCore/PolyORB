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

   Orb : Corba.Orb.Object := Corba.Orb.Orb_Init("omniORB2");
   Boa : Corba.Boa.Object := Corba.Orb.Boa_Init(Orb, "omniORB2_BOA") ;

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
   Put_Line("Test d'attribut par heritage simple:");
   Put_Line("La valeur de la marque est mise a  : ma marque");
   Tank.Set_Mark(T, Corba.To_Corba_String("ma marque"));

   Put_Line("");
   Put_Line("Verification de l'attribution :");
   Put_Line ("   la marque est : "
             & Corba.To_standard_String(Tank.Get_Mark(T)));

   Put_Line("");
   Put_Line("Verification de la valeur de l'attribut du parent :"
            & Corba.To_standard_String(Vehicle.Get_Mark(Vehicle.To_Ref(T))));
   Put_Line("");
   Put_Line("");
   Put_Line("Test de methode par heritage simple :");
   Put_Line("Je peux conduir a 18 ans : "
            &  Corba.Boolean'Image(Tank.Can_drive(T, 18)));
   Put_Line("");
   Put_Line("");
   Put_Line("Test de methode par heritage multiple :");
   Tank.Shoot(T, Distance) ;
   Put_Line("Tank touche ....., ca marche");

   Put_Line("");
   Put_Line("");
   Put_Line("Test de methode par heritage multiple en castant en weapon :");
   W := Weapon.To_Ref(T);
   Weapon.Shoot(W, Distance);
   Put_Line("Weapon touche ..... ca marche");
   Put_Line("");
   Put_Line("Test de cast a partir de weapon en vehicle :");
   V := Vehicle.To_Ref(W);
   Put_Line("  le marque est :" & Corba.To_standard_String(Vehicle.Get_Mark(V)));
   Put_Line("Cast reussi");
   Put_Line("");
   Put_Line("Test de cast du precedent en Object.Ref :");
   O := To_Ref(V);
   Put_Line("Test de cast final de l'Object.Ref en tank:");
   T := Tank.To_Ref(O);
   Put_Line("  le marque est :" & Corba.To_standard_String(Tank.Get_Mark(T)));
   Put_Line("Cast reussi");


   Put_Line("");
   Put_Line("Test complet!");


end Client ;






