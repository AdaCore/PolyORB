with Ada.Command_Line;
with Ada.Text_IO;
with CORBA; use CORBA;
with CORBA.ORB;
with Args;

procedure Client is
   A, B, C  : CORBA.Long;
   S        : CORBA.Long;
   IOR      : CORBA.String;
   MyArgs   : Args.Ref;
begin
   if Ada.Command_Line.Argument_Count < 1 then
      Ada.Text_IO.Put_Line ("usage : client <IOR_string_from_server>");
      return;
   end if;

   ORB.Init ("omniORB2");
   IOR := To_CORBA_String (Ada.Command_Line.Argument (1));
   ORB.String_To_Object (IOR, MyArgs);

   if Args.Is_Nil (MyArgs) then
      Ada.Text_IO.Put_Line ("main : cannot invoke on a nil reference");
      return;
   end if;

   A := 1;
   B := 2;
   C := 3;
   Args.Plus_Minus (MyArgs, A, B, S, C);

   Ada.Text_IO.Put_Line (">> " & A'Img & B'Img);
   Ada.Text_IO.Put_Line ("<< " & S'Img & " " & C'Img);

end Client;
