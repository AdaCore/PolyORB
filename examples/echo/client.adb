with Ada.Command_Line;
with Text_IO; use Text_IO;
with CORBA;
with CORBA.ORB;
with CORBA.BOA;
with CORBA.Object;
with Echo;

with AdaBroker.Exceptions;

procedure Client is
   -- Initialisation of The ORB
   ORB : CORBA.ORB.Object := CORBA.ORB.ORB_Init("omniORB2");

   Sent_Msg, Rcvd_Msg, IOR : CORBA.String;

   MyEcho : Echo.Ref;

begin
   if Ada.Command_Line.Argument_Count < 1 then
      Put_Line ("usage : client <IOR_string_from_server>");
      return;
   end if;

   -- transforms the Ada string into CORBA.String
   IOR := CORBA.To_CORBA_String (Ada.Command_Line.Argument (1));

   -- getting the CORBA.Object
   CORBA.ORB.String_To_Object (IOR, MyEcho);

   -- checking if it worked
   if Echo.Is_Nil (MyEcho) then
      Put_Line ("main : cannot invoke on a nil reference");
      return;
   end if;

   -- sending message
   Sent_Msg := CORBA.To_CORBA_String (Standard.String'("Hello Ada !"));
   Rcvd_Msg := Echo.EchoString (MyEcho, Sent_Msg);

   Put_Line("I said : " & CORBA.To_Standard_String (Sent_Msg) );
   Put_Line("The object answered : " & CORBA.To_Standard_String (Rcvd_Msg));

end Client;
