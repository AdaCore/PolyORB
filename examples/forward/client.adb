with Ada.Command_Line ;
with Text_Io ; use Text_Io ;
with CORBA, CORBA.Orb, CORBA.Boa, CORBA.Object ;
with AdaBroker.Exceptions ;
with Chicken ; use Chicken ;
with Chicken_Forward ;
with Egg ; use Egg ;
with Egg_Forward ;

procedure Client is
   Orb : CORBA.Orb.Object := CORBA.Orb.Orb_Init("omniORB2") ;
   Boa : CORBA.Boa.Object := CORBA.Orb.Boa_Init(Orb, "omniORB2_BOA") ;
   Egg1, Egg2, Egg3, Egg4 : Egg.Ref ;
   Chicken1, Chicken2, Chicken3, Chicken4 : Chicken.Ref ;
   Ef : Egg_Forward.Ref ;
   Ior : CORBA.String ;
   I : CORBA.Unsigned_Short := 0 ;
begin

   Put_Line("MAIN : Starting client") ;

   if Ada.Command_Line.Argument_Count < 1 then
      Put_Line ("usage : client <egg_IOR>") ;
      return ;
   end if ;

   Ior := CORBA.To_Corba_String(Ada.Command_Line.Argument(1)) ;

   CORBA.Orb.String_To_Object(IOR, Egg1) ;

   Put_Line("MAIN : Got the Egg 1") ;
   Put_Line("") ;
   Put_Line("") ;

   Put_Line("######### Trying to hatch  ###########") ;
   Chicken1 := Chicken.Convert_Forward.From_Forward(Hatch(Egg1)) ;
   Put_Line("---> A new chicken is born") ;
   Put_Line("") ;

   Put_Line("######### Trying to lay  ###########") ;
   Lay(Chicken1, I, Ef) ;
   Egg2 := Egg.Convert_Forward.From_Forward(Ef) ;
   Put_Line("---> This chicken has laid its egg number " & CORBA.Unsigned_Short'Image(I)) ;
   Lay(Chicken1, I, Ef) ;
   Egg3 := Egg.Convert_Forward.From_Forward(Ef) ;
   Put_Line("---> This chicken has laid its egg number " & CORBA.Unsigned_Short'Image(I)) ;
   Egg4 := Egg.Convert_Forward.From_Forward(Ef) ;
   Lay(Chicken1, I, Ef) ;
   Put_Line("---> This chicken has laid its egg number " & CORBA.Unsigned_Short'Image(I)) ;
   Put_Line("") ;

   Put_Line("######### Freshly laid eggs are going to hatch ###########") ;
   Chicken2 := Chicken.Convert_Forward.From_Forward(Hatch(Egg2)) ;
   Put_Line("---> A new chicken is born") ;
   Chicken3 := Chicken.Convert_Forward.From_Forward(Hatch(Egg3)) ;
   Put_Line("---> A new chicken is born") ;
   Put_Line("") ;

   Put_Line("######### Can an egg hatch twice ?? ###########") ;
   begin
      Chicken4 := Chicken.Convert_Forward.From_Forward(Hatch(Egg2)) ;
      Put_Line("-->>> Ouups,  Yes, there is a problem !!!") ;
      return ;
   exception
      when Already_Hatched =>
         Put_Line("---> Of course not !! I caught an Already_Hatched exception.") ;
   end ;
   Put_Line("") ;

   Put_Line("######### Can an old chicken still lay ?  ###########") ;
   Lay(Chicken1, I, Ef) ;
   Egg4 :=  Egg.Convert_Forward.From_Forward(Ef) ;
   Put_Line("---> Yes, this chicken has laid its egg number " & CORBA.Unsigned_Short'Image(I)) ;
   Put_Line("") ;

   Put_Line("Everything is all right in the henhouse, let's go and do something else ...") ;
end ;




