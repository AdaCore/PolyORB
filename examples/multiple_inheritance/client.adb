with Ada.Command_Line;
with CORBA.Object;
with CORBA; use CORBA;
with CORBA.ORB;
with Weapon;
with Vehicle;
with Tank;
with Report; use Report;
with Ada.Text_IO;

procedure Client is
   O : CORBA.Object.Ref;
   T : Tank.Ref;
   W : Weapon.Ref;
   V : Vehicle.Ref;
   L : Weapon.Longueur_Array := (10, 15, 12);
   D : Weapon.Dist := (Longueur => L , Largeur => 13);
   M : CORBA.String := To_CORBA_String ("AdaBroker");

   Ok  : Boolean;
   IOR : CORBA.String;
begin

   if Ada.Command_Line.Argument_Count < 1 then
      Ada.Text_IO.Put_Line ("usage : client <IOR_string_from_server>");
      return;
   end if;

   ORB.Init ("omniORB2");

   IOR := CORBA.To_CORBA_String (Ada.Command_Line.Argument (1));
   ORB.String_To_Object (IOR, T);

   Tank.Set_Mark (T, M);
   Output ("test tank mark attribute", Tank.Get_Mark (T) = M);

   V := Vehicle.To_Ref (T);
   Output ("test vehicle mark attribute", Vehicle.Get_Mark (V) = M);

   Output ("test simple inheritance", Tank.Can_drive (T, 18));

   begin
      Ok := True;
      Tank.Shoot (T, D);
   exception when others =>
      Ok := False;
   end;
   Output ("test multiple inheritance", Ok);

   begin
      Ok := True;
      W  := Weapon.To_Ref (T);
      Weapon.Shoot (W, D);
   exception when others =>
      Ok := False;
   end;
   Output ("test widening on first parent", Ok);

   V := Vehicle.To_Ref (W);
   Output ("test widening on second parent", Vehicle.Get_Mark (V) = M);

   O := CORBA.Object.Ref (V);
   T := Tank.To_Ref (O);
   Output ("test widening on CORBA.Object.Ref", Tank.Get_Mark (T) = M);
end Client;
