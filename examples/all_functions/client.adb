with Ada.Command_Line;
with Text_IO; use Text_IO;
with CORBA; use CORBA;
with CORBA.ORB;
with CORBA.BOA;
with CORBA.Object;
with All_Functions; use All_Functions;
with Report; use Report;

procedure Client is

   ORB : CORBA.ORB.Object := CORBA.ORB.ORB_Init ("omniORB2");

   Sent_Msg, Rcvd_Msg, IOR : CORBA.String;

   MyObj : All_Functions.Ref;
   I, J, K, L, M : CORBA.Short;
   Ok : Boolean;

begin

   if Ada.Command_Line.Argument_Count < 1 then
      Put_Line ("usage : client <IOR_string_from_server>");
      return;
   end if;

   IOR := CORBA.To_CORBA_String (Ada.Command_Line.Argument (1));
   CORBA.ORB.String_To_Object (IOR, MyObj);

   Output ("test not nil reference", not Is_Nil (MyObj));

   Set_The_Attribute (MyObj, 24);
   Output ("test attribute", Get_The_Attribute (MyObj) = 24);

   Output ("test readonly attribute", Get_The_Readonly_Attribute (MyObj) = 18);

   begin
      Ok := True;
      Void_Proc (MyObj);
   exception when others =>
      Ok := False;
   end;
   Output ("test void procedure", Ok);

   begin
      In_Proc (MyObj, 1, 2, 3);
      Ok := True;
   exception when others =>
      Ok := False;
   end;
   Output ("test in param procedure", Ok);

   begin
      Ok := False;
      Out_Proc (MyObj, I, J, K);
      Ok := (I = 10) and then (J = 11) and then (K = 12);
   exception when others =>
      null;
   end;
   Output ("test out param procedure", Ok);

   begin
      Ok := False;
      I  := 2;
      J  := 3;
      Inout_Proc (MyObj, I, J);
      Ok := (I = 3 and then J = 4);
   exception when others =>
      null;
   end;
   Output ("test in out param procedure", Ok);

   begin
      Ok := False;
      I := 1;
      J := 2;
      In_Out_Proc (MyObj, 1, 2, I, J);
      Ok := (I = 3 and then J = 4);
   exception when others =>
      null;
   end;
   Output ("test in and out param procedure", Ok);

   begin
      Ok := False;
      I  := -4;
      J  := -5;
      In_Inout_Proc (MyObj, 1, I, 3, J);
      Ok := (I = 36) and then (J = 40);
   exception when others =>
      null;
   end;
   Output ("test in and inout param procedure", Ok);

   begin
      I := -11;
      J := -21;
      K := -31;
      K := -41;
      Out_Inout_Proc (MyObj, I, J, K, L);
      Ok := (I = 45) and then (J = 46) and then (K = 47) and then (L = 48);
   exception when others =>
      null;
   end;
   Output ("test inout and out param procedure", Ok);

   begin
      Ok := False;
      I := 78;
      J := 79;
      In_Out_Inout_Proc (MyObj, 1, I, J);
      Ok := (I = -54) and then (J = 80);
   exception when others =>
      null;
   end;
   Output ("test in and out and inout param procedure", Ok);

   Output ("test void function", Void_Fun (MyObj) = 3);
   Output ("test in param function", In_Fun (MyObj, 1, 2, 3) = 7);

   begin
      Ok := False;
      I := 1;
      J := 2;
      K := 3;
      L := 4;
      Out_Fun (MyObj, I, J, K, L);
      Ok := (I = 5) and then (J = 6) and then (K = 7) and then (L = 10);
   exception when others =>
      null;
   end;
   Output ("test out param function", Ok);

   begin
      Ok := False;
      I := 1;
      J := 2;
      K := 3;
      Inout_Fun (MyObj, I, J, L);
      Ok := (I = 2) and then (J = 3) and then (L = 5);
   exception when others =>
      null;
   end;
   Output ("test inout param function", Ok);

   begin
      Ok := False;
      I := 10;
      J := 11;
      In_Out_Fun (MyObj, 1, 2, I, J, K);
      Ok := (I = 2) and then (J = 1) and then (K = 3);
   exception when others =>
      null;
   end;
   Output ("test in and out param function", Ok);

   begin
      Ok := False;
      I := -1;
      J := -2;
      K := -3;
      In_Inout_Fun (MyObj, -1, I, -2, J, K);
      Ok := (I = -2) and then (J = -4) and then (K = -6);
   exception when others =>
      null;
   end;
   Output ("test in and inout param function", Ok);

   begin
      Ok := False;
      I := -1;
      J := -2;
      K := -3;
      L := -4;
      M := -5;
      Out_Inout_Fun (MyObj, I, J, K, L, M);
      Ok := (I = -2) and then (J = -1) and then (K = -2)
        and then (L = -3) and then (M = -7);
   exception when others =>
      null;
   end;
   Output ("test out and inout param function", Ok);

   begin
      Ok := False;
      I := -1;
      J := -2;
      K := -3;
      In_Out_Inout_Fun (MyObj, 85, I, J, K);
      Ok := (I = 86) and then (J = 83) and then (K = -1);
   exception when others =>
      null;
   end;
   Output ("test in and out and inout param function", Ok);

   begin
      Oneway_Void_Proc (MyObj);
      Ok := True;
   exception when others =>
      Ok := False;
   end;
   Output ("test void one way procedure", Ok);

   begin
      Oneway_In_Proc (MyObj, 1, 2, 3);
      Ok := True;
   exception when others =>
      Ok := False;
   end;
   Output ("test in param one way procedure", Ok);

end Client;




