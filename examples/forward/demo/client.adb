with Ada.Command_Line ;
with Text_Io ; use Text_Io ;
with Corba, Corba.Orb, Corba.Boa, Corba.Object ;
with Exceptions ;
with Chicken ; use Chicken ;
with Chicken_Forward ;
with Egg ; use Egg ;
with Egg_Forward ;

procedure Client is
   Orb : Corba.Orb.Object := Corba.Orb.Orb_Init("omniORB2") ;
   Boa : Corba.Boa.Object := Corba.Orb.Boa_Init(Orb, "omniORB2_BOA") ;
   Egg1, Egg2, Egg3, Egg4 : Egg.Ref ;
   Chicken1, Chicken2, Chicken3 : Chicken.Ref ;
   Ior : Corba.String ;
   I : Corba.Unsigned_Short := 0 ;
begin

   Put_Line("MAIN : Starting client") ;

   if Ada.Command_Line.Argument_Count < 1 then
      Put_Line ("usage : client <egg_IOR>") ;
      return ;
   end if ;

   Ior := Corba.To_Corba_String(Ada.Command_Line.Argument(1)) ;

   Corba.Orb.String_To_Object(IOR, Egg1) ;

   Put_Line("MAIN : Got the Egg 1") ;
   Put_Line("") ;
   Put_Line("") ;

   Put_Line("######### Trying to hatch  ###########") ;
   Chicken1 := Hatch(Egg1) ;
   Put_Line("---> A new chicken is born") ;
   Put_Line("") ;

   Put_Line("######### Trying to lay  ###########") ;
   Egg2 := Lay(Chicken1, I) ;
   Put_Line("---> This chicken has laid its egg number " & Corba.Unsigned_Short'Image(I)) ;
   Egg3 :=  Lay(Chicken1, I) ;
   Put_Line("---> This chicken has laid its egg number " & Corba.Unsigned_Short'Image(I)) ;
   Egg4 :=  Lay(Chicken1, I) ;
   Put_Line("---> This chicken has laid its egg number " & Corba.Unsigned_Short'Image(I)) ;
   Put_Line("") ;

   Put_Line("######### Freshly laid eggs are going to hatch ###########") ;
   Chicken2 := Hatch(Egg2) ;
   Put_Line("---> A new chicken is born") ;
   Chicken3 := Hatch(Egg3) ;
   Put_Line("---> A new chicken is born") ;
   Put_Line("") ;

   Put_Line("######### Can an egg hatch twice ?? ###########") ;
   begin
      Chicken4 := Hatch(Egg2) ;
      Put_Line("-->>> Ouups,  Yes, there is a problem !!!") ;
      return ;
   exception
      when Already_Hatched =>
         Put_Line("---> Of course not !! I caught an Already_Hatched exception.") ;
   end ;
   Put_Line("") ;

   Put_Line("######### Can an old chicken still lay ?  ###########") ;
   Egg4 := Lay(Chicken1, I) ;
   Put_Line("---> Yes, this chicken has laid its egg number " & Corba.Unsigned_Short'Image(I)) ;
   Put_Line("") ;

   Put_Line("Everything is all right in the henhouse, let's go to something else") ;
end ;




