with Ada.Command_Line;
with Ada.Text_IO;

with CORBA;        use CORBA;
with CORBA.Object; use CORBA.Object;

with CORBA.ORB;
with CORBA.Object.OmniORB;

with Helper; use Helper;
with Invoker;


with Report;    use Report;

--  This client is intended to be used with the server of all_types
--  It reproduces most of the tests of all_types/client.adb with some dynamic
--  invocations.
--
--  Have a look to the Helper files too.


procedure Dii_Client is
   Repository_Id : CORBA.String;
   IOR           : CORBA.String;
   Server        : Ref := Object.Nil_Ref;
   Ctx           : Context.Object;
   Nvl           : NVList.Object := NVList.Null_Object;
   Result        : NamedValue;
   Returns       : Status;
   Rq            : Request.Object;
   Argument      : NamedValue;
   X_Any, Y_Any  : Any;

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
      X : CORBA.Boolean := True;
      Y : CORBA.Boolean := False;
   begin
      X_Any := To_Any (X);
      Y_Any := To_Any (Y);
      Invoker.Run ("echoBoolean", Server, Ctx, X_Any, Y_Any);
      Y := From_Any (Y_Any);
      Output ("test boolean", X = Y);
   end;

   --  short
   declare
      X : Short := 123;
      Y : Short := 0;
   begin
      X_Any := To_Any (X);
      Y_Any := To_Any (Y);
      Invoker.Run ("echoShort", Server, Ctx, X_Any, Y_Any);
      Y := From_Any (Y_Any);
      Output ("test short", X = Y);
   end;

   --  long
   declare
      X : Long := 456;
      Y : Long := 0;
   begin
      X_Any := To_Any (X);
      Y_Any := To_Any (Y);
      Invoker.Run ("echoLong", Server, Ctx, X_Any, Y_Any);
      Y := From_Any (Y_Any);
      Output ("test long", X = Y);
   end;

   --  unsigned short
   declare
      X : Unsigned_Short := 456;
      Y : Unsigned_Short := 0;
   begin
      X_Any := To_Any (X);
      Y_Any := To_Any (Y);
      Invoker.Run ("echoUShort", Server, Ctx, X_Any, Y_Any);
      Y := From_Any (Y_Any);
      Output ("test unsigned_short", X = Y);
   end;

   --  unsigned long
   declare
      X : Unsigned_Long := 123;
      Y : Unsigned_Long := 0;
   begin
      X_Any := To_Any (X);
      Y_Any := To_Any (Y);
      Invoker.Run ("echoULong", Server, Ctx, X_Any, Y_Any);
      Y := From_Any (Y_Any);
      Output ("test unsigned_long", X = Y);
   end;

   --  float
   declare
      X : CORBA.Float := 2.7;
      Y : CORBA.Float := 0.0;
   begin
      X_Any := To_Any (X);
      Y_Any := To_Any (Y);
      Invoker.Run ("echoFloat", Server, Ctx, X_Any, Y_Any);
      Y := From_Any (Y_Any);
      Output ("test float", X = Y);
   end;

   --  double
   declare
      X : CORBA.Double := 3.14;
      Y : CORBA.Double := 0.0;
   begin
      X_Any := To_Any (X);
      Y_Any := To_Any (Y);
      Invoker.Run ("echoDouble", Server, Ctx, X_Any, Y_Any);
      Y := From_Any (Y_Any);
      Output ("test double", X = Y);
   end;

   --  char
   declare
      X : CORBA.Char := 'A';
      Y : CORBA.Char := 'Z';
   begin
      X_Any := To_Any (X);
      Y_Any := To_Any (Y);
      Invoker.Run ("echoChar", Server, Ctx, X_Any, Y_Any);
      Y := From_Any (Y_Any);
      Output ("test char", X = Y);
   end;

   --  octet
   declare
      X : CORBA.Octet := 5;
      Y : CORBA.Octet := 0;
   begin
      X_Any := To_Any (X);
      Y_Any := To_Any (Y);
      Invoker.Run ("echoOctet", Server, Ctx, X_Any, Y_Any);
      Y := From_Any (Y_Any);
      Output ("test octet", X = Y);
   end;

   --  string
   declare
      Y : CORBA.String := To_CORBA_String ("");
      X : CORBA.String := To_CORBA_String ("hello");
   begin
      X_Any := To_Any (X);
      Y_Any := To_Any (Y);
      Invoker.Run ("echoString", Server, Ctx, X_Any, Y_Any);
      Y := From_Any (Y_Any);
      Output ("test string", X = Y);
   end;

   -- union
   declare
      X : Helper.Example := (Switch => 2, Flags => False);
      Y : Helper.Example := (Switch => 1, Counter => 2706);
   begin
      X_Any := Helper.To_Any (X);
      Y_Any := Helper.To_Any (Y);
      Invoker.Run ("echo1", Server, Ctx, X_Any, Y_Any);
      Y := Helper.From_Any (Y_Any);
      Output ("test union", X = Y);
   end;


   --  simple_array
   declare
      X : Helper.Simple_Array := (0,1,2,3,4,5,6,7,8,9);
      Y : Helper.Simple_Array := (9,9,9,9,9,9,9,9,9,9);
   begin
      X_Any := Helper.To_Any (X);
      Y_Any := Helper.To_Any (Y);
      Invoker.Run ("echoArray", Server, Ctx, X_Any, Y_Any);
      Y := Helper.From_Any (Y_Any);
      Output ("test simple array", X = Y);
   end;


   --  simple structure
   declare
      X : Helper.Simple_Struct := (A => (0,1,2,3,4,5,6,7,8,9), B => 15);
      Y : Helper.Simple_Struct := (A => (0,0,0,0,0,0,0,0,0,0), B => 0);
   begin
      X_Any := Helper.To_Any (X);
      Y_Any := Helper.To_Any (Y);
      Invoker.Run ("echo2", Server, Ctx, X_Any, Y_Any);
      Y := Helper.From_Any (Y_Any);
      Output ("test simple structure", Y = X);
   end;

   --  enumeration
   declare
      X : Helper.Color := Blue;
      Y : Helper.Color := Red;
   begin
      X_Any := Helper.To_Any (X);
      Y_Any := Helper.To_Any (Y);
      Invoker.Run ("echo3", Server, Ctx, X_Any, Y_Any);
      Y := Helper.From_Any (Y_Any);
      Output ("test enumeration", X = Y);
   end;

   --  sequences
   declare
      Tc : TypeCode.Object;
      Arg_Any, Res_Any : Any;
   begin
      --  build the typecode
      TypeCode.Set(Tc, Tk_Sequence);
      TypeCode.Add_Parameter
        (Tc, TypeCode.To_Any (TypeCode.TC_Short));  --  type of elements
      TypeCode.Add_Parameter
        (Tc, To_Any (CORBA.Unsigned_Long (0)));  --  unbounded sequence
      --  build the any
      Arg_Any := Prepare_Any_From_Agregate_Tc (Tc);
      for I in 0 .. 9 loop
         Add_Agregate_Any_Member (Arg_Any, To_Any (CORBA.Short (I)));
      end loop;
      --  build the argument
      Argument := (To_CORBA_String ("arg"), Arg_Any, 0, ARG_IN);
      --  build shell for the returned value
      Res_Any := Prepare_Any_From_Agregate_Tc (Tc);
      Result := (To_CORBA_String ("res"), Res_Any, 0, ARG_OUT);
      --  build an invoke request
      Create_Request (Server,
                      Ctx,
                      To_CORBA_String ("echo6"),
                      Nvl,
                      Result,
                      Rq,
                      0,
                      Returns);
      --  add argument
      Request.Add_Arg (Rq, Argument);
      --  invoke
      Request.Invoke (Rq, 0);
      --  get result
      Result :=  Request.Return_Value (Rq);
      --  check result
      declare
         Len_Res : CORBA.Long :=
           Any_Agregate_Size (Result.Argument);
         Ok_Res : CORBA.Boolean := True;
      begin
         for I in 0 .. Len_Res - 1 loop
            Ok_Res := Ok_Res
              and Short (I) =
              From_Any (Get_Any_Agregate_Member (Result.Argument,
                                                 TypeCode.TC_Short,
                                                 I));
         end loop;
         Output ("test sequence", Ok_Res);
      end;
   end;


   --  arrays (1)
   declare
      X : Line
        := ((Switch => 1, Counter => 19),
            (Switch => 2, Flags => True),
            (Switch => 3, Unknown => 25));
      Y : Line
        := ((Switch => 1, Counter => 0),
            (Switch => 2, Flags => False),
            (Switch => 3, Unknown => 0));
   begin
      X_Any := Helper.To_Any (X);
      Y_Any := Helper.To_Any (Y);
      Invoker.Run ("echo8", Server, Ctx, X_Any, Y_Any);
      Y := Helper.From_Any (Y_Any);
      Output ("test array (1)", X = Y);
   end;


   --  array (2)
   declare
      X : Helper.Square
        := (((A => (0,1,2,3,4,5,6,7,8,9), B=> 23),
             (A => (9,8,7,6,5,4,3,2,1,0), B=> 17)),
            ((A => (0,1,2,3,4,5,6,7,8,9), B=> 23),
             (A => (9,8,7,6,5,4,3,2,1,0), B=> 17)));
      Y : Helper.Square
        := (((A => (0,0,0,0,0,0,0,0,0,0), B=> 0),
             (A => (0,0,0,0,0,0,0,0,0,0), B=> 0)),
            ((A => (0,0,0,0,0,0,0,0,0,0), B=> 0),
             (A => (0,0,0,0,0,0,0,0,0,0), B=> 0)));
   begin
      X_Any := Helper.To_Any (X);
      Y_Any := Helper.To_Any (Y);
      Invoker.Run ("echo9", Server, Ctx, X_Any, Y_Any);
      Y := Helper.From_Any (Y_Any);
      Output ("test array (2)", X = Y);
   end;

   --  array (3)
   declare
      X : Helper.Cube
        := (((To_CORBA_String (Standard.String'("case1")),
              To_CORBA_String (Standard.String'("case2"))),
             (To_CORBA_String (Standard.String'("case3")),
              To_CORBA_String (Standard.String'("case4")))),
            ((To_CORBA_String (Standard.String'("case5")),
              To_CORBA_String (Standard.String'("case6"))),
             (To_CORBA_String (Standard.String'("case7")),
              To_CORBA_String (Standard.String'("case8")))));
      Y : Helper.Cube
        := (((To_CORBA_String (Standard.String'("gloups1")),
              To_CORBA_String (Standard.String'("gloups2"))),
             (To_CORBA_String (Standard.String'("gloups3")),
              To_CORBA_String (Standard.String'("gloups4")))),
            ((To_CORBA_String (Standard.String'("gloups5")),
              To_CORBA_String (Standard.String'("gloups6"))),
             (To_CORBA_String (Standard.String'("gloups7")),
              To_CORBA_String (Standard.String'("gloups8")))));
   begin
      X_Any := Helper.To_Any (X);
      Y_Any := Helper.To_Any (Y);
      Invoker.Run ("echo10", Server, Ctx, X_Any, Y_Any);
      Y := Helper.From_Any (Y_Any);
      Output ("test array (3)", X = Y);
   end;

   --  reference
   declare
      X : Ref := Server;
      Y : Ref := CORBA.Object.Nil_Ref;
   begin
      X_Any := Object.To_Any (X);
      Y_Any := Object.To_Any (Y);
      Invoker.Run ("echo11", Server, Ctx, X_Any, Y_Any);
      Y := Object.From_Any (Y_Any);
      Output ("test reference", Is_Equivalent (X, Y));
   end;

end Dii_Client;
