with Ada.Command_Line;
with Ada.Text_IO;

with CORBA;        use CORBA;
with CORBA.Object; use CORBA.Object;

with CORBA.ORB;

with All_Types; use All_Types;
with Report;    use Report;

procedure Client is
   IOR : CORBA.String;
   MyAll_Types : All_Types.Ref;
   Ok : Boolean;
begin
   if Ada.Command_Line.Argument_Count < 1 then
      Ada.Text_IO.Put_Line ("usage : client <IOR_string_from_server>");
      return;
   end if;

   ORB.Init ("omniORB2");
   IOR := To_CORBA_String (Ada.Command_Line.Argument (1)) ;
   ORB.String_To_Object (IOR, MyAll_Types);

   Output ("test not null", not All_Types.Is_Nil (MyAll_Types));
   Output ("test boolean", EchoBoolean (MyAll_Types, True) = True);
   Output ("test short", EchoShort (MyAll_Types, 123) = 123);
   Output ("test long",  EchoLong (MyAll_Types, 456) = 456);
   Output ("test unsigned_short", EchoUShort (MyAll_Types, 456) = 456);
   Output ("test unsigned_long", EchoULong (MyAll_Types, 123) = 123);
   Output ("test float", EchoFloat (MyAll_Types, 2.7) = 2.7);
   Output ("test double", EchoDouble (MyAll_Types, 3.14) = 3.14);
   Output ("test char", EchoChar (MyAll_Types, 'A') = 'A');
   Output ("test octet", EchoOctet (MyAll_Types, 5) = 5);
   Output ("test string",
           EchoString (MyAll_Types, To_CORBA_String ("hello")) =
           To_CORBA_String ("hello"));

   begin
      Ok := False;
      Simple_Exception_Test (MyAll_Types);
   exception
      when Simple_Exception =>
         Ok := True;
      when others =>
         null;
   end;
   Output ("test simple exception", Ok);

   declare
      Member : Complexe_Exception_Members;
   begin
      Ok := False;
      Complexe_Exception_Test (MyAll_Types);
   exception
      when E : Complexe_Exception =>
         Get_Members (E, Member);
         Ok := (Member.Excep = 21);
   end;
   Output ("test complexe exception", Ok);

   declare
      X : Example := (Switch => 2, Flags => True);
   begin
      Output ("test union", Echo1 (MyAll_Types, X) = X);
   end;


   declare
      X : Simple_Array := (0,1,2,3,4,5,6,7,8,9);
   begin
      Output ("test simple array", EchoArray (MyAll_Types, X) = X);
   end;

   declare
      X : Simple_Struct := (A => (0,1,2,3,4,5,6,7,8,9), B => 10);
   begin
      Output ("test simple structure", Echo2 (MyAll_Types, X) = X);
   end;

   Output ("test enumeration", Echo3 (MyAll_Types, Blue) = Blue);

   declare
      X : U_Sequence := U_Sequence (IDL_SEQUENCE_Short.Null_Sequence);
   begin
      X := X & 1 & 2 & 3 & 4 & 5;
      Output ("test unbounded sequence", Echo6 (MyAll_Types, X) = X);
   end;

   -- bounded sequences
   declare
      X : B_Sequence := B_Sequence (IDL_SEQUENCE_Long_1.Null_Sequence);
   begin
      X := X & 1 & 2 & 3 & 4 & 5;
      Output ("test bounded sequence",  Echo7 (MyAll_Types, X) = X);
   end;

   Output ("test readonly attribute",
           Get_R_Attribute (MyAll_Types) = Blue);

   declare
      X : Example := (Switch => 1, Counter => 23);
   begin
      Output ("test default attribute",
              Get_N_Attribute (MyAll_Types) = X);
   end;

   declare
      X : Example := (Switch => 2, Flags => True);
   begin
      Set_N_Attribute (MyAll_Types, X);
      Output ("test updated attribute",
              Get_N_Attribute (MyAll_Types) = X);
   end;

   declare
      X : All_Types.Line
        := ((Switch => 1, Counter => 19),
            (Switch => 2, Flags => True),
            (Switch => 3, Unknown => 25));
   begin
      Output ("test arrays (1)", Echo8 (MyAll_Types, X) = X);
   end;

   declare
      X : Square
        := (((A => (0,1,2,3,4,5,6,7,8,9), B=> 23),
             (A => (9,8,7,6,5,4,3,2,1,0), B=> 17)),
            ((A => (0,1,2,3,4,5,6,7,8,9), B=> 23),
             (A => (9,8,7,6,5,4,3,2,1,0), B=> 17)));
   begin
      Output ("test arrays (2)", Echo9 (MyAll_Types, X) = X);
   end;

   declare
      X : Cube
        := (((To_CORBA_String (Standard.String'("case1")),
              To_CORBA_String (Standard.String'("case2"))),
             (To_CORBA_String (Standard.String'("case3")),
              To_CORBA_String (Standard.String'("case4")))),
            ((To_CORBA_String (Standard.String'("case5")),
              To_CORBA_String (Standard.String'("case6"))),
             (To_CORBA_String (Standard.String'("case7")),
              To_CORBA_String (Standard.String'("case8")))));
   begin
      Output ("test arrays (3)", Echo10 (MyAll_Types, X) = X);
   end;

   declare
      X : All_Types.Ref;
      Y : Example := (Switch => 2, Flags => False);
   begin
      Set_N_Attribute (Myall_Types, Y);
      X := Echo11 (MyAll_Types, Myall_Types);
      Output ("test reference", Get_N_Attribute (X) = Y);
   end;

   declare
      X : CORBA.Object.Ref := CORBA.Object.Ref (To_Ref (Myall_Types));
   begin
      Output ("test CORBA.Object.Ref",
         Is_Equivalent (Echo12 (MyAll_Types, X), X));
   end;

   declare
      X : All_Types.Ref;
   begin
      X := Get_Myself (MyAll_Types);
      Output ("test self reference", Is_Equivalent (Get_Myself (X), X));
   end;
end Client;
