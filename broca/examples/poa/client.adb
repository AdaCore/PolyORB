--   echo client.
with Ada.Command_Line;
with Text_IO; use Text_IO;
with CORBA; use CORBA;
with CORBA.ORB;
with Echo;

procedure Client is
   --  Initialisation of The ORB
   --  Orb : CORBA.Orb.Object := CORBA.Orb.Orb_Init("broca");

   --  Initialisation of the BOA
   --   Boa : CORBA.Boa.Object := CORBA.Orb.Boa_Init(Orb, "omniORB2_BOA") ;

   Sent_Msg, Rcvd_Msg, IOR : CORBA.String;

   myecho : Echo.Ref;

begin

   if Ada.Command_Line.Argument_Count < 1 then
      Put_Line ("usage : client <IOR_string_from_server>");
      return;
   end if;

   --  transforms the Ada string into CORBA.String
   IOR := CORBA.To_CORBA_String (Ada.Command_Line.Argument (1));

   --  getting the CORBA.Object
   CORBA.ORB.String_To_Object (IOR, myecho);

   --  checking if it worked
   if Echo.Is_Nil (myecho) then
      Put_Line ("main : cannot invoke on a nil reference");
      return;
   end if;

   --  sending message
   Sent_Msg := CORBA.To_CORBA_String (Standard.String'("Hello Ada !"));
   --  Sent_Msg := CORBA.To_CORBA_String (Standard.String'("Hello!"));
   Rcvd_Msg := Echo.echoString (myecho, Sent_Msg);

   --  printing result
   Put_Line ("I said : " & CORBA.To_Standard_String (Sent_Msg));
   Put_Line ("The object answered : " & CORBA.To_Standard_String (Rcvd_Msg));
exception
   when E : CORBA.Transient =>
      declare
         Memb : System_Exception_Members;
      begin
         Get_Members (E, Memb);
         Put ("received exception transient, minor");
         Put (Unsigned_Long'Image (Memb.Minor));
         Put (", completion status: ");
         Put_Line (Completion_Status'Image (Memb.Completed));
      end;
   when E : Echo.no_walls =>
      declare
         Memb : Echo.no_walls_Members;
      begin
         Echo.Get_Members (E, Memb);
         Put_Line ("sorry, no wall." & CORBA.Unsigned_Long'Image (Memb.one));
      end;
end Client;
