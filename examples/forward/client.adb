with Ada.Command_Line;
with Text_IO; use Text_IO;
with CORBA; use CORBA;
with CORBA.ORB;
with CORBA.BOA;
with CORBA.Object;
with AdaBroker.Exceptions;
with Chicken; use Chicken;
with Chicken_Forward;
with Egg; use Egg;
with Egg_Forward;
with Report; use Report;

procedure Client is
   ORB : CORBA.ORB.Object := CORBA.ORB.ORB_Init ("omniORB2");
   BOA : CORBA.BOA.Object := CORBA.ORB.BOA_Init (ORB, "omniORB2_BOA");

   ER : Egg.Ref;
   CR : Chicken.Ref;
   EF : Egg_Forward.Ref;
   IOR : CORBA.String;
   N : CORBA.Unsigned_Short := 0;
   Ok : Boolean;
begin

   if Ada.Command_Line.Argument_Count < 1 then
      Put_Line ("usage : client <egg_IOR>");
      return;
   end if;

   IOR := CORBA.To_Corba_String (Ada.Command_Line.Argument (1));

   CORBA.ORB.String_To_Object (IOR, ER);

   CR := Chicken.Convert_Forward.From_Forward (Hatch (ER));
   Output ("a new chicken is born", not Is_Nil (CR));

   for I in 1 .. 3 loop
      Lay (CR, N, EF);
      ER := Egg.Convert_Forward.From_Forward (EF);
      Output ("the new chicken has laid an egg",
              N = 1 and then not Is_Nil (ER));
      CR := Chicken.Convert_Forward.From_Forward (Hatch (ER));
      Output ("a new chicken is born", not Is_Nil (CR));
   end loop;

   begin
      Ok := False;
      CR := Chicken.Convert_Forward.From_Forward (Hatch (ER));
   exception
      when Already_Hatched =>
         Ok := True;
      when others =>
         null;
   end;
   Output ("detect illegal forward operation", Ok);

   Lay (CR, N, EF);
   ER :=  Egg.Convert_Forward.From_Forward (EF);
   Output ("the old chicken can still lay an egg",
           N = 1 and then not Is_Nil (ER));
end;




