with Ada.Command_Line;
with Ada.Text_IO;
with CORBA; use CORBA;
with CORBA.ORB;
with Mystruct;

procedure Client is
   IOR      : CORBA.String;
   My_St    : Mystruct.Ref;
   S1, S2   : Mystruct.Simple_Struct;
begin
   if Ada.Command_Line.Argument_Count < 1 then
      Ada.Text_IO.Put_Line ("usage : client <IOR_string_from_server>");
      return;
   end if;

   ORB.Init ("omniORB2");
   IOR := To_CORBA_String (Ada.Command_Line.Argument (1));
   ORB.String_To_Object (IOR, My_St);

   if Mystruct.Is_Nil (My_St) then
      Ada.Text_IO.Put_Line ("main : cannot invoke on a nil reference");
      return;
   end if;

   S1.A := To_CORBA_String ("coucou");
   S1.B := CORBA.Long (2311);

   S2 := Mystruct.EchoStruct (My_St, S1);

   Ada.Text_IO.Put_Line (">> " & To_Standard_String (S1.A)
                         & " " & S1.B'Img);
   Ada.Text_IO.Put_Line ("<< " & To_Standard_String (S2.A)
                         & " " & S2.B'Img);

end Client;
