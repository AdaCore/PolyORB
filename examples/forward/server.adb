with Ada.Command_Line ;
with Text_Io ; use Text_Io ;
with CORBA ; use CORBA ;
with CORBA.Object ;
with CORBA.Orb ; use CORBA.Orb ;
with CORBA.Boa ; use CORBA.Boa ;
with AdaBroker.Exceptions ;
with Chicken ;
with Chicken_Forward ;
with Egg ;
with Egg_Forward ;
with Egg.Impl ;
with Chicken.Impl ;

procedure Server is
   Orb : CORBA.Orb.Object := CORBA.Orb.Orb_Init("omniORB2") ;
   Boa : CORBA.Boa.Object := CORBA.Orb.Boa_Init(Orb, "omniORB2_BOA") ;
   Myegg : Egg.Impl.Object ;
   Mychicken : Chicken.Impl.Object ;
   Ior : CORBA.String ;
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




