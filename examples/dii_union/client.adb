with Ada.Command_Line;
with Ada.Text_IO;
with CORBA; use CORBA;
with CORBA.ORB;
with Modifier;

procedure Client is
   IOR      : CORBA.String;
   Modif    : Modifier.Ref;
   S1       : Modifier.Example (2);
   S2       : Modifier.Example;
begin
   if Ada.Command_Line.Argument_Count < 1 then
      Ada.Text_IO.Put_Line ("usage : client <IOR_string_from_server>");
      return;
   end if;

   ORB.Init ("omniORB2");
   IOR := To_CORBA_String (Ada.Command_Line.Argument (1));
   ORB.String_To_Object (IOR, Modif);

   if Modifier.Is_Nil (Modif) then
      Ada.Text_IO.Put_Line ("main : cannot invoke on a nil reference");
      return;
   end if;

   S1.Flag := False;

   S2 := Modifier.Modify (Modif, S1);

   Ada.Text_IO.Put_Line (">> sw  2 -> flag = False");
   case S2.Switch is
      when 2 =>
         Ada.Text_IO.Put_Line ("<< sw = 2 -> flag = " & S2.Flag'Img);
      when 1 =>
         Ada.Text_IO.Put_Line ("<< sw = 1 => counter = " & S2.Counter'Img);
      when others =>
         Ada.Text_IO.Put_Line ("<< sw = others => unknown = "
                               & S2.Unknown'Img);
   end case;

end Client;
