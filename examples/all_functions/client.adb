with Ada.Command_Line;
with Ada.Text_IO;

with CORBA; use CORBA;
with CORBA.ORB;

with all_functions; use all_functions;
with Report; use Report;

with PolyORB.Setup.CORBA_Client;
pragma Warnings (Off, PolyORB.Setup.CORBA_Client);

procedure Client is
   IOR : CORBA.String;
   MyObj : all_functions.Ref;
   I, J, K, L, M : CORBA.Short;
   Ok : Boolean;
begin
   CORBA.ORB.Initialize ("ORB");

   if Ada.Command_Line.Argument_Count < 1 then
      Ada.Text_IO.Put_Line ("usage : client <IOR_string_from_server>");
      return;
   end if;

   --  transforms the Ada string into CORBA.String
   IOR := CORBA.To_CORBA_String (Ada.Command_Line.Argument (1));

   --  getting the CORBA.Object
   CORBA.ORB.String_To_Object (IOR, MyObj);

   --  checking if it worked
   if all_functions.Is_Nil (MyObj) then
      Ada.Text_IO.Put_Line ("main : cannot invoke on a nil reference");
      return;
   end if;

   Output ("test not nil reference", not Is_Nil (MyObj));

--    Set_The_Attribute (MyObj, 24);
--    Output ("test attribute", Get_The_Attribute (MyObj) = 24);

--    Output ("test readonly attribute",
--      Get_The_Readonly_Attribute (MyObj) = 18);

   begin
      Ok := True;
      void_proc (MyObj);
   exception when others =>
      Ok := False;
   end;
   Output ("test void procedure", Ok);

   begin
      in_proc (MyObj, 1, 2, 3);
      Ok := True;
   exception when others =>
      Ok := False;
   end;
   Output ("test in param procedure", Ok);

   begin
      Ok := False;
      out_proc (MyObj, I, J, K);
      Ok := (I = 10) and then (J = 11) and then (K = 12);
   exception when others =>
      null;
   end;
   Output ("test out param procedure", Ok);

   begin
      Ok := False;
      I  := 2;
      J  := 3;
      inout_proc (MyObj, I, J);
      Ok := (I = 3 and then J = 4);
   exception when others =>
      null;
   end;
   Output ("test in out param procedure", Ok);

   begin
      Ok := False;
      I := 1;
      J := 2;
      in_out_proc (MyObj, 1, 2, I, J);
      Ok := (I = 3 and then J = 4);
   exception when others =>
      null;
   end;
   Output ("test in and out param procedure", Ok);

   begin
      Ok := False;
      I  := -4;
      J  := -5;
      in_inout_proc (MyObj, 1, I, 3, J);
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
      out_inout_proc (MyObj, I, J, K, L);
      Ok := (I = 45) and then (J = 46) and then (K = 47) and then (L = 48);
   exception when others =>
      null;
   end;
   Output ("test inout and out param procedure", Ok);

   begin
      Ok := False;
      I := 78;
      J := 79;
      in_out_inout_proc (MyObj, 1, I, J);
      Ok := (I = -54) and then (J = 80);
   exception when others =>
      null;
   end;
   Output ("test in and out and inout param procedure", Ok);

   Output ("test void function", void_fun (MyObj) = 3);
   Output ("test in param function", in_fun (MyObj, 1, 2, 3) = 7);

   begin
      Ok := False;
      I := 1;
      J := 2;
      K := 3;
      L := 4;
      out_fun (MyObj, I, J, K, L);
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
      inout_fun (MyObj, I, J, L);
      Ok := (I = 2) and then (J = 3) and then (L = 5);
   exception when others =>
      null;
   end;
   Output ("test inout param function", Ok);

   begin
      Ok := False;
      I := 10;
      J := 11;
      in_out_fun (MyObj, 1, 2, I, J, K);
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
      in_inout_fun (MyObj, -1, I, -2, J, K);
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
      out_inout_fun (MyObj, I, J, K, L, M);
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
      in_out_inout_fun (MyObj, 85, I, J, K);
      Ok := (I = 86) and then (J = 83) and then (K = -1);
   exception when others =>
      null;
   end;
   Output ("test in and out and inout param function", Ok);

   begin
      oneway_void_proc (MyObj);
      delay 1.0;
      Ok := oneway_checker (MyObj) = 1;
      if Ok then
         delay 5.0;
         Ok := oneway_checker (MyObj) = 2;
      end if;
   exception when others =>
      Ok := False;
   end;
   Output ("test void one way procedure", Ok);

   begin
      oneway_in_proc (MyObj, 10, 20);
      delay 1.0;
      Ok := oneway_checker (MyObj) = 10;
      if Ok then
         delay 5.0;
         Ok := oneway_checker (MyObj) = 20;
      end if;
   exception when others =>
      Ok := False;
   end;
   Output ("test in param one way procedure", Ok);

end Client;




