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

   T : Tank.Ref ;
   W : Weapon.Ref;

   Ior : Corba.String ;
begin

   if Ada.Command_Line.Argument_Count < 1 then
      Put_Line ("usage : client <IOR_string_from_server>") ;
      return ;
   end if ;

   -- transforms the Ada string into Corba.String
   IOR := Corba.To_Corba_String(Ada.Command_Line.Argument(1)) ;
   Corba.Orb.String_To_Object(IOR, T) ;

   Put_Line("");
   Put_Line("");
   Put_Line("Test de methode par heritage multiple :");
   Tank.Shoot(T, 10) ;
   Put_Line("Tank touche ....., ca marche");

   Put_Line("");
   Put_Line("Test de methode par heritage multiple en castant :");
   W := Weapon.To_Ref(T);
   Weapon.Shoot(W, 5);
   Put_Line("Weapon touche ..... ca marche");

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
   Put_Line("Test complet!");


end Client ;






