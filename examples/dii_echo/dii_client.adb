with Ada.Command_Line;
with Ada.Text_IO;
with CORBA; use CORBA;
with CORBA.ORB;
with CORBA.Object.OmniORB;

with CORBA.Context;
with CORBA.Request;
with CORBA.NVList;
with CORBA.Object;

procedure Dii_Client is
   Repository_Id : CORBA.String;
   Sent_Msg : CORBA.String;
   Rcvd_Msg : CORBA.String;
   IOR      : CORBA.String;
   MyEcho   : Object.Ref;
   Ctx      : Context.Object;
   Nvl      : NVList.Object := NVList.Null_Object;
   Result   : NamedValue;
   Returns  : CORBA.Status;
   Rq       : Request.Object;
   Argument : NamedValue;
begin
   if Ada.Command_Line.Argument_Count < 1 then
      Ada.Text_IO.Put_Line ("usage : client <IOR_string_from_server>");
      return;
   end if;

   Repository_Id := To_CORBA_String ("IDL:Echo:1.0");
   Object.OmniORB.Register (Repository_Id, Object.Nil_Ref, null);

   ORB.Init ("omniORB2");
   IOR := To_CORBA_String (Ada.Command_Line.Argument (1));
   ORB.String_To_Object (IOR, MyEcho);

   if Object.Is_Nil (MyEcho) then
      Ada.Text_IO.Put_Line ("main : cannot invoke on a nil reference");
      return;
   end if;

   Sent_Msg := To_CORBA_String ("Dynamic hello AdaBroker!");

   Ada.Text_IO.Put_Line ("Client building request");
   Rcvd_Msg := To_CORBA_String ("");
   Result := (To_CORBA_String ("res"),
              To_Any (Rcvd_Msg),
              0,
              ARG_OUT);

   Object.Create_Request (MyEcho,
                          Ctx,
                          To_CORBA_String ("echoString"),
                          Nvl,
                          Result,
                          Rq,
                          0,
                          Returns);

   Argument := (To_CORBA_String ("arg1"),
                To_Any (Sent_Msg), 0, ARG_IN);


   Ada.Text_IO.Put_Line ("Client adding argument");
   Request.Add_Arg (Rq, Argument);

   Ada.Text_IO.Put_Line ("Client invoking request");
   Request.Invoke (Rq, 0);

   Ada.Text_IO.Put_Line ("Client extracting result");
   Result := Request.Return_Value (Rq);
   Rcvd_Msg := From_Any (Result.Argument);
   Ada.Text_IO.Put_Line (">> " & To_Standard_String (Sent_Msg));
   Ada.Text_IO.Put_Line ("<< " & To_Standard_String (Rcvd_Msg));


end Dii_Client;
