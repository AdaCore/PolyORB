with Ada.Command_Line ;
with Text_Io ; use Text_Io ;
with Corba ; use Corba ;
with Corba.Object ;
with Corba.Orb ; use Corba.Orb ;
with Corba.Boa ; use Corba.Boa ;
with Exceptions ;
with Chicken ;
with Chicken_Forward ;
with Egg ;
with Egg_Forward ;
with Egg.Impl ;
with Chicken.Impl ;

procedure Server is
   Orb : Corba.Orb.Object := Corba.Orb.Orb_Init("omniORB2") ;
   Boa : Corba.Boa.Object := Corba.Orb.Boa_Init(Orb, "omniORB2_BOA") ;
   Myegg : Egg.Impl.Object ;
   Mychicken : Chicken.Impl.Object ;
   Ior : Corba.String ;
begin

   Put_Line("MAIN : Starting server") ;

   Chicken.Impl.Set_Boa(Mychicken, Boa) ;
   Egg.Impl.Set_Boa(Myegg, Boa) ;

   Object_Is_Ready(Boa, Myegg) ;
   Object_Is_Ready(Boa, Mychicken) ;

   Ior := Object_To_String(Myegg) ;
   Put_Line("'" & To_Standard_String(Ior) & "'") ;

   Implementation_Is_Ready(Boa) ;

end Server;




