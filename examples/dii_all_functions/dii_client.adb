with Ada.Command_Line;
with Ada.Text_IO;

with CORBA; use CORBA;
with CORBA.Object;
with CORBA.NVList;
with CORBA.Request;

with CORBA.ORB;
with CORBA.Object.OmniORB;

with Report;    use Report;


--  This client is intended to work with the server of all_functions
--  It reproduces part of the tests of the static all_functions client with
--  some dynamic invocations
--  Exceptions are not handled properly (unlike in static invocation) since
--  they are not supported yet in DII

procedure Dii_Client is
   Repository_Id : CORBA.String;
   IOR           : CORBA.String;
   Server        : Object.Ref := Object.Nil_Ref;
   Ctx           : Context.Object;
   Results       : NVList.Object := NVList.Null_Object;
   Void_Result, Result   : NamedValue;
   Returns       : Status;
   Rq            : Request.Object;
   Arg1, Arg2, Arg3, arg4   : NamedValue;
   X1_Any, X2_Any, X3_Any, X4_Any, Y_Any : Any;
   It : NVList.Iterator;
   Ok : CORBA.Boolean;
begin

   if Ada.Command_Line.Argument_Count < 1 then
      Ada.Text_IO.Put_Line ("usage : client <IOR_string_from_server>");
      return;
   end if;

   Repository_Id := To_CORBA_String ("IDL:all_functions:1.0");
   Object.OmniORB.Register (Repository_Id, Object.Nil_Ref, null);

   ORB.Init ("omniORB2");
   IOR := To_CORBA_String (Ada.Command_Line.Argument (1)) ;
   ORB.String_To_Object (IOR, Server);

   --  not null
   Output ("test not nil reference", not Object.Is_Nil (Server));


   --  set and get the attribute
   declare
      X : CORBA.Short := 24;
      Y : CORBA.Short := 0;
   begin
      --  set
      X1_Any := To_Any (X);
      Arg1 := (To_CORBA_String ("arg"), X1_Any, 0, ARG_IN);
      Result := Void_Result;
      Object.Create_Request (Server,
                             Ctx,
                             To_CORBA_String ("_set_the_attribute"),
                             NVList.Null_Object,
                             Result,
                             Rq,
                             0,
                             Returns);
      Request.Add_Arg (Rq, Arg1);
      Request.Invoke (Rq, 0);
      -- get
      Y_Any := To_Any (Y);
      Result := (To_CORBA_String ("res"), Y_Any, 0, ARG_OUT);
      Object.Create_Request (Server,
                             Ctx,
                             To_CORBA_String ("_get_the_attribute"),
                             NVList.Null_Object,
                             Result,
                             Rq,
                             0,
                             Returns);
      Request.Invoke (Rq, 0);
      Result :=  Request.Return_Value (Rq);
      Y := From_Any (Result.Argument);
      --  test
      Output ("test attribute", X = Y);
   end;

   --  read only attribute
   declare
      Y : CORBA.Short := 0;
   begin
      Y_Any := To_Any (Y);
      Result := (To_CORBA_String ("res"), Y_Any, 0, ARG_OUT);
      Object.Create_Request (Server,
                             Ctx,
                             To_CORBA_String ("_get_the_readonly_attribute"),
                             NVList.Null_Object,
                             Result,
                             Rq,
                             0,
                             Returns);
      Request.Invoke (Rq, 0);
      Result :=  Request.Return_Value (Rq);
      Y := From_Any (Result.Argument);
      Output ("test readonly attribute", Y = 18);
   end;

   --  void procedure
   begin
      Result := Void_Result;
      Object.Create_Request (Server,
                             Ctx,
                             To_CORBA_String ("void_proc"),
                             NVList.Null_Object,
                             Result,
                             Rq,
                             0,
                             Returns);
      Request.Invoke (Rq, 0);
      --  cannot catch exception in DII, so assume that if we reach this point
      --  then evrything was ok
      Output ("test void procedure", True);
   end;

   --  in param
   begin
      X1_Any := To_Any (CORBA.Short (1));
      Arg1 := (To_CORBA_String ("a"), X1_Any, 0, ARG_IN);
      X2_Any := To_Any (CORBA.Short (2));
      Arg2 := (To_CORBA_String ("b"), X2_Any, 0, ARG_IN);
      X3_Any := To_Any (CORBA.Short (3));
      Arg3 := (To_CORBA_String ("c"), X3_Any, 0, ARG_IN);
      Result := Void_Result;
      Object.Create_Request (Server,
                             Ctx,
                             To_CORBA_String ("in_proc"),
                             NVList.Null_Object,
                             Result,
                             Rq,
                             0,
                             Returns);
      Request.Add_Arg (Rq, Arg1);
      Request.Add_Arg (Rq, Arg2);
      Request.Add_Arg (Rq, Arg3);
      Request.Invoke (Rq, 0);
      --  same as void procedure...
      Output ("test in param procedure", True);
   end;

   --  out param
   begin
      X1_Any := To_Any (CORBA.Short (1));
      Arg1 := (To_CORBA_String ("a"), X1_Any, 0, ARG_OUT);
      X2_Any := To_Any (CORBA.Short (2));
      Arg2 := (To_CORBA_String ("b"), X2_Any, 0, ARG_OUT);
      X3_Any := To_Any (CORBA.Short (3));
      Arg3 := (To_CORBA_String ("c"), X3_Any, 0, ARG_OUT);
      Result := Void_Result;
      Object.Create_Request (Server,
                             Ctx,
                             To_CORBA_String ("out_proc"),
                             NVList.Null_Object,
                             Result,
                             Rq,
                             0,
                             Returns);
      Request.Add_Arg (Rq, Arg1);
      Request.Add_Arg (Rq, Arg2);
      Request.Add_Arg (Rq, Arg3);
      Request.Invoke (Rq, 0);
      Results := Request.Return_Arguments (Rq);
      NVList.Start (It, Results);
      Ok := True;
      Ok := Ok and From_Any (NVList.Get (It).Argument) = CORBA.Short (10);
      NVList.Next (It);
      Ok := Ok and From_Any (NVList.Get (It).Argument) = CORBA.Short (11);
      NVList.Next (It);
      Ok := Ok and From_Any (NVList.Get (It).Argument) = CORBA.Short (12);
      Output ("test out param procedure", Ok);
   end;

   --  in out param
   begin
      X1_Any := To_Any (CORBA.Short (2));
      Arg1 := (To_CORBA_String ("a"), X1_Any, 0, ARG_INOUT);
      X2_Any := To_Any (CORBA.Short (3));
      Arg2 := (To_CORBA_String ("b"), X2_Any, 0, ARG_INOUT);
      Result := Void_Result;
      Object.Create_Request (Server,
                             Ctx,
                             To_CORBA_String ("inout_proc"),
                             NVList.Null_Object,
                             Result,
                             Rq,
                             0,
                             Returns);
      Request.Add_Arg (Rq, Arg1);
      Request.Add_Arg (Rq, Arg2);
      Request.Invoke (Rq, 0);
      Results := Request.Return_Arguments (Rq);
      NVList.Start (It, Results);
      Ok := True;
      Ok := Ok and From_Any (NVList.Get (It).Argument) = CORBA.Short (3);
      NVList.Next (It);
      Ok := Ok and From_Any (NVList.Get (It).Argument) = CORBA.Short (4);
      Output ("test in out param procedure", Ok);
   end;

   -- in and out param
   begin
      X1_Any := To_Any (CORBA.Short (1));
      Arg1 := (To_CORBA_String ("a"), X1_Any, 0, ARG_IN);
      X2_Any := To_Any (CORBA.Short (2));
      Arg2 := (To_CORBA_String ("b"), X2_Any, 0, ARG_IN);
      X3_Any := To_Any (CORBA.Short (1));
      Arg3 := (To_CORBA_String ("c"), X3_Any, 0, ARG_OUT);
      X4_Any := To_Any (CORBA.Short (2));
      Arg4 := (To_CORBA_String ("d"), X4_Any, 0, ARG_OUT);
      Result := Void_Result;
      Object.Create_Request (Server,
                             Ctx,
                             To_CORBA_String ("in_out_proc"),
                             NVList.Null_Object,
                             Result,
                             Rq,
                             0,
                             Returns);
      Request.Add_Arg (Rq, Arg1);
      Request.Add_Arg (Rq, Arg2);
      Request.Add_Arg (Rq, Arg3);
      Request.Add_Arg (Rq, Arg4);
      Request.Invoke (Rq, 0);
      Results := Request.Return_Arguments (Rq);
      Ok := True;
      NVList.Start (It, Results);
      NVList.Next (It);  --  skip the first 2 qrguments
      NVList.Next (It);
      Ok := Ok and From_Any (NVList.Get (It).Argument) = CORBA.Short (3);
      NVList.Next (It);
      Ok := Ok and From_Any (NVList.Get (It).Argument) = CORBA.Short (4);
      Output ("test in and out param procedure", Ok);
   end;

   --  in and inout param
   begin
      X1_Any := To_Any (CORBA.Short (1));
      Arg1 := (To_CORBA_String ("a"), X1_Any, 0, ARG_IN);
      X2_Any := To_Any (CORBA.Short (-4));
      Arg2 := (To_CORBA_String ("b"), X2_Any, 0, ARG_INOUT);
      X3_Any := To_Any (CORBA.Short (3));
      Arg3 := (To_CORBA_String ("c"), X3_Any, 0, ARG_IN);
      X4_Any := To_Any (CORBA.Short (-5));
      Arg4 := (To_CORBA_String ("d"), X4_Any, 0, ARG_INOUT);
      Result := Void_Result;
      Object.Create_Request (Server,
                             Ctx,
                             To_CORBA_String ("in_inout_proc"),
                             NVList.Null_Object,
                             Result,
                             Rq,
                             0,
                             Returns);
      Request.Add_Arg (Rq, Arg1);
      Request.Add_Arg (Rq, Arg2);
      Request.Add_Arg (Rq, Arg3);
      Request.Add_Arg (Rq, Arg4);
      Request.Invoke (Rq, 0);
      Results := Request.Return_Arguments (Rq);
      Ok := True;
      NVList.Start (It, Results);
      NVList.Next (It);  --  skip the first arg
      Ok := Ok and From_Any (NVList.Get (It).Argument) = CORBA.Short (36);
      NVList.Next (It);
      NVList.Next (It); --  skip the third arg
      Ok := Ok and From_Any (NVList.Get (It).Argument) = CORBA.Short (40);
      Output ("test in and inout param procedure", Ok);
   end;

   --  inout and out param
   begin
      X1_Any := To_Any (CORBA.Short (-11));
      Arg1 := (To_CORBA_String ("a"), X1_Any, 0, ARG_OUT);
      X2_Any := To_Any (CORBA.Short (-21));
      Arg2 := (To_CORBA_String ("b"), X2_Any, 0, ARG_INOUT);
      X3_Any := To_Any (CORBA.Short (-31));
      Arg3 := (To_CORBA_String ("c"), X3_Any, 0, ARG_INOUT);
      X4_Any := To_Any (CORBA.Short (-41));
      Arg4 := (To_CORBA_String ("d"), X4_Any, 0, ARG_OUT);
      Result := Void_Result;
      Object.Create_Request (Server,
                             Ctx,
                             To_CORBA_String ("out_inout_proc"),
                             NVList.Null_Object,
                             Result,
                             Rq,
                             0,
                             Returns);
      Request.Add_Arg (Rq, Arg1);
      Request.Add_Arg (Rq, Arg2);
      Request.Add_Arg (Rq, Arg3);
      Request.Add_Arg (Rq, Arg4);
      Request.Invoke (Rq, 0);
      Results := Request.Return_Arguments (Rq);
      Ok := True;
      NVList.Start (It, Results);
      Ok := Ok and From_Any (NVList.Get (It).Argument) = CORBA.Short (45);
      NVList.Next (It);
      Ok := Ok and From_Any (NVList.Get (It).Argument) = CORBA.Short (46);
      NVList.Next (It);
      Ok := Ok and From_Any (NVList.Get (It).Argument) = CORBA.Short (47);
      NVList.Next (It);
      Ok := Ok and From_Any (NVList.Get (It).Argument) = CORBA.Short (48);
      Output ("test inout and out param procedure", Ok);
   end;

   --  in and out and inout
   begin
      X1_Any := To_Any (CORBA.Short (1));
      Arg1 := (To_CORBA_String ("a"), X1_Any, 0, ARG_IN);
      X2_Any := To_Any (CORBA.Short (78));
      Arg2 := (To_CORBA_String ("b"), X2_Any, 0, ARG_OUT);
      X3_Any := To_Any (CORBA.Short (79));
      Arg3 := (To_CORBA_String ("c"), X3_Any, 0, ARG_INOUT);
      Result := Void_Result;
      Object.Create_Request (Server,
                             Ctx,
                             To_CORBA_String ("in_out_inout_proc"),
                             NVList.Null_Object,
                             Result,
                             Rq,
                             0,
                             Returns);
      Request.Add_Arg (Rq, Arg1);
      Request.Add_Arg (Rq, Arg2);
      Request.Add_Arg (Rq, Arg3);
      Request.Invoke (Rq, 0);
      Results := Request.Return_Arguments (Rq);
      Ok := True;
      NVList.Start (It, Results);
      NVList.Next (It);
      Ok := Ok and From_Any (NVList.Get (It).Argument) = CORBA.Short (-54);
      NVList.Next (It);
      Ok := Ok and From_Any (NVList.Get (It).Argument) = CORBA.Short (80);
      Output ("test in and out and inout param procedure", Ok);
   end;

   -- void function
   begin
      Y_Any := To_Any (CORBA.Short (0));
      Result := (To_CORBA_String ("res"), Y_Any, 0, ARG_OUT);
      Object.Create_Request (Server,
                             Ctx,
                             To_CORBA_String ("void_fun"),
                             NVList.Null_Object,
                             Result,
                             Rq,
                             0,
                             Returns);
      Request.Invoke (Rq, 0);
      Result := Request.Return_Value (Rq);
      Output
        ("test void function", From_Any(Result.Argument) = CORBA.Short (3));
   end;

   --  in param function
   begin
      X1_Any := To_Any (CORBA.Short (1));
      Arg1 := (To_CORBA_String ("a"), X1_Any, 0, ARG_IN);
      X2_Any := To_Any (CORBA.Short (2));
      Arg2 := (To_CORBA_String ("b"), X2_Any, 0, ARG_IN);
      X3_Any := To_Any (CORBA.Short (3));
      Arg3 := (To_CORBA_String ("c"), X3_Any, 0, ARG_IN);
      Y_Any := To_Any (CORBA.Short (0));
      Result := (To_CORBA_String ("res"), Y_Any, 0, ARG_OUT);
      Object.Create_Request (Server,
                             Ctx,
                             To_CORBA_String ("in_fun"),
                             NVList.Null_Object,
                             Result,
                             Rq,
                             0,
                             Returns);
      Request.Add_Arg (Rq, Arg1);
      Request.Add_Arg (Rq, Arg2);
      Request.Add_Arg (Rq, Arg3);
      Request.Invoke (Rq, 0);
      Result := Request.Return_Value (Rq);
      Output
        ("test in param function",
         From_Any(Result.Argument) = CORBA.Short (7));
   end;

   --  out param function
   --  like in static invocation, a function with some out/inout parameters
   --  is considered as a procedure (the return value becomes an out parameter)
   begin
      X1_Any := To_Any (CORBA.Short (1));
      Arg1 := (To_CORBA_String ("a"), X1_Any, 0, ARG_OUT);
      X2_Any := To_Any (CORBA.Short (2));
      Arg2 := (To_CORBA_String ("b"), X2_Any, 0, ARG_OUT);
      X3_Any := To_Any (CORBA.Short (3));
      Arg3 := (To_CORBA_String ("c"), X3_Any, 0, ARG_OUT);
      X4_Any := To_Any (CORBA.Short (4));
      Arg4 := (To_CORBA_String ("d"), X4_Any, 0, ARG_OUT);
      Result := Void_Result;
      Object.Create_Request (Server,
                             Ctx,
                             To_CORBA_String ("out_fun"),
                             NVList.Null_Object,
                             Result,
                             Rq,
                             0,
                             Returns);
      Request.Add_Arg (Rq, Arg1);
      Request.Add_Arg (Rq, Arg2);
      Request.Add_Arg (Rq, Arg3);
      Request.Add_Arg (Rq, Arg4);
      Results := Request.Return_Arguments (Rq);
      Request.Invoke (Rq, 0);
      Ok := True;
      NVList.Start (It, Results);
      Ok := Ok and From_Any (NVList.Get (It).Argument) = CORBA.Short (5);
      NVList.Next (It);
      Ok := Ok and From_Any (NVList.Get (It).Argument) = CORBA.Short (6);
      NVList.Next (It);
      Ok := Ok and From_Any (NVList.Get (It).Argument) = CORBA.Short (7);
      NVList.Next (It);
      Ok := Ok and From_Any (NVList.Get (It).Argument) = CORBA.Short (10);
      Output ("test out param function", Ok);
   end;

   --  since the tests done for static invocation for functions with different
   --  parameters mode are equivalent to the procedure ones, I don't reproduce
   --  them here

   --  I also don't test oneway procedure since it is not yet supported in DII

end Dii_Client;
