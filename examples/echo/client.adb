with Ada.Command_Line;
with Ada.Text_IO;
with CORBA; use CORBA;
with CORBA.ORB;
with Echo;

procedure Client is
   Sent_Msg : CORBA.String;
   Rcvd_Msg : CORBA.String;
   IOR      : CORBA.String;
   MyEcho   : Echo.Ref;
begin
   if Ada.Command_Line.Argument_Count < 1 then
      Ada.Text_IO.Put_Line ("usage : client <IOR_string_from_server>");
      return;
   end if;

   ORB.Init ("omniORB2");
   IOR := To_CORBA_String (Ada.Command_Line.Argument (1));
   ORB.String_To_Object (IOR, MyEcho);

   if Echo.Is_Nil (MyEcho) then
      Ada.Text_IO.Put_Line ("main : cannot invoke on a nil reference");
      return;
   end if;

   Sent_Msg := To_CORBA_String ("Hello AdaBroker!");
   Rcvd_Msg := Echo.EchoString (MyEcho, Sent_Msg);

   Ada.Text_IO.Put_Line (">> " & To_Standard_String (Sent_Msg));
   Ada.Text_IO.Put_Line ("<< " & To_Standard_String (Rcvd_Msg));
end Client;
