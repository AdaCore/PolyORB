with Ada.Command_Line;
with Ada.Text_IO;

with CORBA;        use CORBA;
with CORBA.Object; use CORBA.Object;

with CORBA.ORB;
with CORBA.Object.OmniORB;

with Report;    use Report;

procedure Dii_Client is
   Repository_Id : CORBA.String;
   IOR           : CORBA.String;
   Server        : Ref;
   Ctx           : Context.Object;
   Nvl           : NVList.Object := NVList.Null_Object;
   Result        : NamedValue;
   Returns       : Status;
   Rq            : Request.Object;
   Argument      : NamedValue;

begin

   if Ada.Command_Line.Argument_Count < 1 then
      Ada.Text_IO.Put_Line ("usage : client <IOR_string_from_server>");
      return;
   end if;

   Repository_Id := To_CORBA_String ("IDL:all_types:1.0");
   Object.OmniORB.Register (Repository_Id, Object.Nil_Ref, null);

   ORB.Init ("omniORB2");
   IOR := To_CORBA_String (Ada.Command_Line.Argument (1)) ;
   ORB.String_To_Object (IOR, Server);


   --  not null
   Output ("test not null", not Is_Nil (Server));

   --  boolean
   declare
      Rcvd_Msg : Boolean := False;
   begin
      Result := (To_CORBA_String ("res"),
                 To_Any (Rcvd_Msg),
                 0,
                 ARG_OUT);
      Create_Request (Server,
                      Ctx,
                      To_CORBA_String ("echoBoolean"),
                   Nvl,
                      Result,
                      Rq,
                      0,
                      Returns);
      Argument := (To_CORBA_String ("arg"),
                   To_Any (True), 0, ARG_IN);
      Request.Add_Arg (Rq, Argument);
      Request.Invoke (Rq, 0);
      Result := Request.Return_Value (Rq);
      Rcvd_Msg := From_Any (Result.Argument);
      Output ("test boolean", Rcvd_Msg = True);
   end;

   --  short
   declare
      Rcvd_Msg : Short := 0;
   begin
      Result := (To_CORBA_String ("res"),
                 To_Any (Rcvd_Msg),
                 0,
                 ARG_OUT);
      Create_Request (Server,
                      Ctx,
                      To_CORBA_String ("echoShort"),
                      Nvl,
                      Result,
                      Rq,
                      0,
                      Returns);
      Argument := (To_CORBA_String ("arg"),
                   To_Any (CORBA.Short (123)), 0, ARG_IN);
      Request.Add_Arg (Rq, Argument);
      Request.Invoke (Rq, 0);
      Result := Request.Return_Value (Rq);
      Rcvd_Msg := From_Any (Result.Argument);
      Output ("test short", Rcvd_Msg = 123);
   end;

   --  long
   declare
      Rcvd_Msg : Long := 0;
   begin
      Result := (To_CORBA_String ("res"),
                 To_Any (Rcvd_Msg),
                 0,
                 ARG_OUT);
      Create_Request (Server,
                      Ctx,
                      To_CORBA_String ("echoLong"),
                      Nvl,
                      Result,
                      Rq,
                      0,
                      Returns);
      Argument := (To_CORBA_String ("arg"),
                   To_Any (CORBA.Long (456)), 0, ARG_IN);
      Request.Add_Arg (Rq, Argument);
      Request.Invoke (Rq, 0);
      Result := Request.Return_Value (Rq);
      Rcvd_Msg := From_Any (Result.Argument);
      Output ("test long", Rcvd_Msg = 456);
   end;

   --  unsigned short
   declare
      Rcvd_Msg : Unsigned_Short := 0;
   begin
      Result := (To_CORBA_String ("res"),
                 To_Any (Rcvd_Msg),
                 0,
                 ARG_OUT);
      Create_Request (Server,
                      Ctx,
                      To_CORBA_String ("echoUShort"),
                      Nvl,
                      Result,
                      Rq,
                      0,
                      Returns);
      Argument := (To_CORBA_String ("arg"),
                   To_Any (CORBA.Unsigned_Short (456)), 0, ARG_IN);
      Request.Add_Arg (Rq, Argument);
      Request.Invoke (Rq, 0);
      Result := Request.Return_Value (Rq);
      Rcvd_Msg := From_Any (Result.Argument);
      Output ("test unsigned_short", Rcvd_Msg = 456);
   end;

   --  unsigned long
   declare
      Rcvd_Msg : Unsigned_Long := 0;
   begin
      Result := (To_CORBA_String ("res"),
                 To_Any (Rcvd_Msg),
                 0,
                 ARG_OUT);
      Create_Request (Server,
                      Ctx,
                      To_CORBA_String ("echoULong"),
                      Nvl,
                      Result,
                      Rq,
                      0,
                      Returns);
      Argument := (To_CORBA_String ("arg"),
                   To_Any (CORBA.Unsigned_Long (123)), 0, ARG_IN);
      Request.Add_Arg (Rq, Argument);
      Request.Invoke (Rq, 0);
      Result := Request.Return_Value (Rq);
      Rcvd_Msg := From_Any (Result.Argument);
      Output ("test unsigned_long", Rcvd_Msg = 123);
   end;

   --  float
   declare
      Rcvd_Msg : CORBA.Float := 0.0;
   begin
      Result := (To_CORBA_String ("res"),
                 To_Any (Rcvd_Msg),
                 0,
                 ARG_OUT);
      Create_Request (Server,
                      Ctx,
                      To_CORBA_String ("echoFloat"),
                      Nvl,
                      Result,
                      Rq,
                      0,
                      Returns);
      Argument := (To_CORBA_String ("arg"),
                   To_Any (CORBA.Float (2.7)), 0, ARG_IN);
      Request.Add_Arg (Rq, Argument);
      Request.Invoke (Rq, 0);
      Result := Request.Return_Value (Rq);
      Rcvd_Msg := From_Any (Result.Argument);
      Output ("test float", Rcvd_Msg = 2.7);
   end;

   --  double
   declare
      Rcvd_Msg : CORBA.Double := 0.0;
   begin
      Result := (To_CORBA_String ("res"),
                 To_Any (Rcvd_Msg),
                 0,
                 ARG_OUT);
      Create_Request (Server,
                      Ctx,
                      To_CORBA_String ("echoDouble"),
                      Nvl,
                      Result,
                      Rq,
                      0,
                      Returns);
      Argument := (To_CORBA_String ("arg"),
                   To_Any (CORBA.Double (3.14)), 0, ARG_IN);
      Request.Add_Arg (Rq, Argument);
      Request.Invoke (Rq, 0);
      Result := Request.Return_Value (Rq);
      Rcvd_Msg := From_Any (Result.Argument);
      Output ("test double", Rcvd_Msg = 3.14);
   end;

   --  char
   declare
      Rcvd_Msg : CORBA.Char := 'Z';
   begin
      Result := (To_CORBA_String ("res"),
                 To_Any (Rcvd_Msg),
                 0,
                 ARG_OUT);
      Create_Request (Server,
                      Ctx,
                      To_CORBA_String ("echoChar"),
                      Nvl,
                      Result,
                      Rq,
                      0,
                      Returns);
      Argument := (To_CORBA_String ("arg"),
                   To_Any ('A'), 0, ARG_IN);
      Request.Add_Arg (Rq, Argument);
      Request.Invoke (Rq, 0);
      Result := Request.Return_Value (Rq);
      Rcvd_Msg := From_Any (Result.Argument);
      Output ("test char", Rcvd_Msg = 'A');
   end;

   --  octet
   declare
      Rcvd_Msg : CORBA.Octet := 0;
   begin
      Result := (To_CORBA_String ("res"),
                 To_Any (Rcvd_Msg),
                 0,
                 ARG_OUT);
      Create_Request (Server,
                      Ctx,
                      To_CORBA_String ("echoOctet"),
                      Nvl,
                      Result,
                      Rq,
                      0,
                      Returns);
      Argument := (To_CORBA_String ("arg"),
                   To_Any (CORBA.Octet (5)), 0, ARG_IN);
      Request.Add_Arg (Rq, Argument);
      Request.Invoke (Rq, 0);
      Result := Request.Return_Value (Rq);
      Rcvd_Msg := From_Any (Result.Argument);
      Output ("test octet", Rcvd_Msg = 5);
   end;

   --  string
   declare
      Rcvd_Msg : CORBA.String := To_CORBA_String ("");
      Sent_Msg : CORBA.String;
   begin
      Result := (To_CORBA_String ("res"),
                 To_Any (Rcvd_Msg),
                 0,
                 ARG_OUT);
      Create_Request (Server,
                      Ctx,
                      To_CORBA_String ("echoString"),
                      Nvl,
                      Result,
                      Rq,
                      0,
                      Returns);
      Sent_Msg := To_CORBA_String ("hello");
      Argument := (To_CORBA_String ("arg"),
                   To_Any (Sent_Msg),
                   0, ARG_IN);
      Request.Add_Arg (Rq, Argument);
      Request.Invoke (Rq, 0);
      Result := Request.Return_Value (Rq);
      Rcvd_Msg := From_Any (Result.Argument);
      Output ("test string", Rcvd_Msg = To_CORBA_String ("hello"));
   end;

   --  EXCEPTIONS TESTS TO DO HERE

   --  union
   declare
      type example(Switch : CORBA.Long := CORBA.Long'First) is
         record
            case Switch is
               when 1 =>
                  Counter : CORBA.Long;
               when 2 =>
                  Flags : CORBA.Boolean;
               when others =>
                  Unknown : CORBA.Long;
            end case;
         end record;
      X : Example := (Switch => 2, Flags => True);
   begin
      null;
   end;


end Dii_Client;
