with Ada.Command_Line;
with Ada.Text_IO;
with CORBA; use CORBA;
with CORBA.Object;
with CORBA.ORB;
with CORBA.BOA;
with AdaBroker.Exceptions;
with Chicken;
with Chicken_Forward;
with Egg;
with Egg_Forward;
with Egg.Impl;
with Chicken.Impl;

procedure Server is
   MyEgg : Egg.Impl.Object;
   MyChicken : Chicken.Impl.Object;
   IOR : CORBA.String;
begin
   ORB.Init ("omniORB2");
   BOA.Init ("omniORB2_BOA");

   BOA.Object_Is_Ready (MyEgg);
   BOA.Object_Is_Ready (MyChicken);

   IOR := ORB.Object_To_String (Egg.To_Ref (MyEgg));
   Ada.Text_IO.Put_Line ("'" & To_Standard_String (IOR) & "'");

   BOA.Impl_Is_Ready;
end Server;
