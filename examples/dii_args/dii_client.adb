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
   A, B, P, M : CORBA.Long;
   IOR      : CORBA.String;
   MyArgs   : Object.Ref;
   Ctx      : CORBA.Context.Object;
   Nvl      : CORBA.NVList.Object := CORBA.NVList.Null_Object;
   Result   : CORBA.NamedValue;
   Returns  : CORBA.Status;
   Rq       : CORBA.Request.Object;
   Argument : CORBA.NamedValue;
   It       : CORBA.NVList.Iterator;
begin

   if Ada.Command_Line.Argument_Count < 1 then
      Ada.Text_IO.Put_Line ("usage : client <IOR_string_from_server>");
      return;
   end if;

   Repository_Id := To_CORBA_String ("IDL:Args:1.0");
   Object.OmniORB.Register (Repository_Id, Object.Nil_Ref, null);

   ORB.Init ("omniORB2");
   IOR := To_CORBA_String (Ada.Command_Line.Argument (1));
   ORB.String_To_Object (IOR, MyArgs);

   if Object.Is_Nil (MyArgs) then
      Ada.Text_IO.Put_Line ("main : cannot invoke on a nil reference");
      return;
   end if;

   A := 1;
   B := 2;
   P := 0;
   M := 0;

   Ada.Text_IO.Put_Line ("Client building request");

   --  result is void by default
   Object.Create_Request (MyArgs,
                          Ctx,
                          CORBA.To_CORBA_String ("Plus_Minus"),
                          Nvl,
                          Result,
                          Rq,
                          0,
                          Returns);


   Ada.Text_IO.Put_Line ("Client adding arguments");

   Argument := (To_CORBA_String ("A"),
                To_Any (A), 0, CORBA.ARG_IN);
   CORBA.Request.Add_Arg (Rq, Argument);
   Argument := (To_CORBA_String ("B"),
                To_Any (B), 0, CORBA.ARG_IN);
   CORBA.Request.Add_Arg (Rq, Argument);
   Argument := (To_CORBA_String ("P"),
                To_Any (P), 0, CORBA.ARG_OUT);
   CORBA.Request.Add_Arg (Rq, Argument);
   Argument := (To_CORBA_String ("M"),
                To_Any (M), 0, CORBA.ARG_OUT);
   CORBA.Request.Add_Arg (Rq, Argument);

   Ada.Text_IO.Put_Line ("Client invoking request");
   CORBA.Request.Invoke (Rq, 0);

   Ada.Text_IO.Put_Line ("Client extracting result");

   Nvl := CORBA.Request.Return_Arguments (Rq);
   CORBA.NVList.Start (It, Nvl);

   A := CORBA.From_Any (CORBA.NVList.Get (It).Argument);
   CORBA.NVList.Next (It);
   B := CORBA.From_Any (CORBA.NVList.Get (It).Argument);
   CORBA.NVList.Next (It);
   P := CORBA.From_Any (CORBA.NVList.Get (It).Argument);
   CORBA.NVList.Next (It);
   M := CORBA.From_Any (CORBA.NVList.Get (It).Argument);

   Ada.Text_IO.Put_Line (">> " & A'Img & " " & B'Img);
   Ada.Text_IO.Put_Line ("<< " & P'Img & " " & M'Img);


end Dii_Client;
