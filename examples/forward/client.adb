----------------------------------------------------------------------------
----                                                                    ----
----     This is a hand-written example using the forward library       ----
----                                                                    ----
----                                                                    ----
----                author : Fabien Azavant                             ----
----                                                                    ----
----------------------------------------------------------------------------


with Ada.Command_Line ;
with Text_Io ; use Text_Io ;
with Corba, Corba.Orb, Corba.Boa, Corba.Object ;
with Exceptions ;
with Chicken ;
with Chicken_Forward ;
with Egg ;
with Egg_Forward ;

procedure Client is
   Orb : Corba.Orb.Object := Corba.Orb.Orb_Init("omniORB2") ;
   Boa : Corba.Boa.Object := Corba.Orb.Boa_Init(Orb, "omniORB2_BOA") ;
   My_Egg : Egg.Ref ;
   My_Chicken : Chicken.Ref ;
   Egg_Ior : Corba.String ;
   Tmp_Chicken : Chicken_Forward.Ref ;
   Tmp_Egg : Egg_Forward.Ref ;
begin
   Put_Line("MAIN : Starting client") ;

   if Ada.Command_Line.Argument_Count < 1 then
      Put_Line ("usage : client <egg_IOR>") ;
      return ;
   end if ;

   Egg_Ior := Corba.To_Corba_String(Ada.Command_Line.Argument(1)) ;

   Corba.Orb.String_To_Object(Egg_IOR, My_Egg) ;

   Put_Line("MAIN : Got the Egg") ;

   Tmp_Chicken := Egg.Hatch(My_Egg) ;

   Put_Line("MAIN : Got the chicken_forward !!!!!!!!!!!") ;

   My_Chicken := Chicken.Convert_Forward.From_Forward(Tmp_Chicken) ;

   Put_Line("MAIN : Got the chicken") ;

   Tmp_Egg := Chicken.Lay(My_Chicken) ;

   Put_Line("MAIN : Got the egg_forward !!!!!!!!!!!") ;

   My_Egg := Egg.Convert_Forward.From_Forward(Tmp_Egg) ;

   Put_Line("MAIN : Got the EGG") ;



end ;




