----------------------------------------------------------------------------
----                                                                    ----
----     This in a hand-written client for All_Types example             ----
----                                                                    ----
----     It provides a declaration of each simple type with the         ----
----     echo function associated.                                      ----
----                                                                    ----
----                                                                    ----
----                server                                              ----
----                                                                    ----
----                authors : Fabien Azavant, Sebastien Ponce           ----
----                                                                    ----
----------------------------------------------------------------------------



with Ada.Command_Line ;
with Text_Io ; use Text_Io ;
with Corba, Corba.Orb, Corba.Boa, Corba.Object ;
with All_Types ; use All_Types ;


procedure Client is

   Orb : Corba.Orb.Object := Corba.Orb.Orb_Init("omniORB2");
   Boa : Corba.Boa.Object := Corba.Orb.Boa_Init(Orb, "omniORB2_BOA") ;

   IOR : Corba.String ;
   MyAll_Types : All_Types.Ref ;

   ----------------------------
   -- Some display functions --
   ----------------------------

   procedure Put (Str : in Simple_Struct) is
   begin
      Put ("( a = (" & Corba.Long'Image(Str.A(0)));
      for I in 1 .. 9 loop
         Put ("," & Corba.Long'Image(Str.A(I))) ;
      end loop ;
      Put (" ) and b = " & Corba.Long'Image(Str.B)) ;
      Put (" )") ;
   end;

   procedure Put (Ex : in Example) is
   begin
      Put ("( switch = " &
           Corba.Long'Image(Ex.Switch) &
           " and") ;
      case Ex.Switch is
         when 1 => Put (" Counter = " & Corba.Long'Image(Ex.Counter)) ;
         when 2 => Put (" Flags = " & Corba.Boolean'Image(Ex.Flags)) ;
         when others => Put (" Unknown = " & Corba.Long'Image(Ex.Unknown)) ;
      end case ;
      Put (" )") ;
   end;

   procedure Put (Seq : in U_Sequence) is
   begin
      for I in 1..Length(Seq) loop
         Put (" " & Corba.Short'Image(Element_Of(Seq,I))) ;
      end loop ;
   end ;

   procedure Put (Seq : in B_Sequence) is
   begin
      for I in 1..Length(Seq) loop
         Put (" " & Corba.Long'Image(Element_Of(Seq,I))) ;
      end loop ;
   end ;

   procedure Put (St : in Corba.String) is
   begin
      Put (Corba.To_Standard_String(St)) ;
   end ;


begin

   Put_Line("main : Starting client") ;

   if Ada.Command_Line.Argument_Count < 1 then
      Put_Line ("usage : client <IOR_string_from_server>") ;
      return ;
   end if ;

   -- transforms the Ada string into Corba.String
   IOR := Corba.To_Corba_String(Ada.Command_Line.Argument(1)) ;

   -- getting the Corba.Object
   Corba.Orb.String_To_Object(IOR, MyAll_Types) ;
   Put_Line("main : Got the Corba.Object") ;

   -- checking if it worked
   if All_Types.Is_Nil(MyAll_Types) then
      Put_Line("main : cannot invoke on a nil reference") ;
      return ;
   end if ;
   Put_Line("main : Ok : Corba.Object is not nil") ;


   ----------------------
   -- using the Object --
   ----------------------

   Put_Line ("") ;
   Put_Line ("") ;


   ------------------
   -- simple types --
   ------------------

   -- Boolean
   declare
      Arg : Corba.Boolean := True ;
   begin
   Put_Line ("####### Test of Boolean #######") ;
   Put_Line("I sent : " & Corba.Boolean'Image(Arg)) ;
   Arg := EchoBoolean(MyAll_Types, Arg) ;
   Put_Line("I received : " & Corba.Boolean'Image(Arg)) ;
   Put_Line("") ;
   Put_Line("") ;
   end ;

   -- Short
   declare
      Arg : Corba.Short := 123 ;
   begin
   Put_Line ("####### Test of Short #######") ;
   Put_Line("I sent : " & Corba.Short'Image(Arg)) ;
   Arg := EchoShort(MyAll_Types, Arg) ;
   Put_Line("I received : " & Corba.Short'Image(Arg)) ;
   Put_Line("") ;
   Put_Line("") ;
   end ;

   -- Long
   declare
      Arg : Corba.Long := 456 ;
   begin
   Put_Line ("####### Test of Long #######") ;
   Put_Line("I sent : " & Corba.Long'Image(Arg)) ;
   Arg := EchoLong(MyAll_Types, Arg) ;
   Put_Line("I received : " & Corba.Long'Image(Arg)) ;
   Put_Line("") ;
   Put_Line("") ;
   end ;

   -- unsigned_short
   declare
      Arg : Corba.Unsigned_Short := 123 ;
   begin
   Put_Line ("####### Test of Unsigned_Short #######") ;
   Put_Line("I sent : " & Corba.Unsigned_Short'Image(Arg)) ;
   Arg := EchoUShort(MyAll_Types, Arg) ;
   Put_Line("I received : " & Corba.Unsigned_Short'Image(Arg)) ;
   Put_Line("") ;
   Put_Line("") ;
   end ;

   -- unsigned_long
   declare
      Arg : Corba.Unsigned_Long := 123 ;
   begin
   Put_Line ("####### Test of Unsigned_Long #######") ;
   Put_Line("I sent : " & Corba.Unsigned_Long'Image(Arg)) ;
   Arg := EchoULong(MyAll_Types, Arg) ;
   Put_Line("I received : " & Corba.Unsigned_Long'Image(Arg)) ;
   Put_Line("") ;
   Put_Line("") ;
   end ;

   -- float
   declare
      Arg : Corba.Float := 1.5 ;
   begin
   Put_Line ("####### Test of Float #######") ;
   Put_Line("I sent : " & Corba.Float'Image(Arg)) ;
   Arg := EchoFloat(MyAll_Types, Arg) ;
   Put_Line("I received : " & Corba.Float'Image(Arg)) ;
   Put_Line("") ;
   Put_Line("") ;
   end ;

   -- double
   declare
      Arg : Corba.Double := 3.14 ;
   begin
   Put_Line ("####### Test of Double #######") ;
   Put_Line("I sent : " & Corba.Double'Image(Arg)) ;
   Arg := EchoDouble(MyAll_Types, Arg) ;
   Put_Line("I received : " & Corba.Double'Image(Arg)) ;
   Put_Line("") ;
   Put_Line("") ;
   end ;

   -- char
   declare
      Arg : Corba.Char := 'A' ;
   begin
   Put_Line ("####### Test of Char #######") ;
   Put_Line("I sent : " & Corba.Char'Image(Arg)) ;
   Arg := EchoChar(MyAll_Types, Arg) ;
   Put_Line("I received : " & Corba.Char'Image(Arg)) ;
   Put_Line("") ;
   Put_Line("") ;
   end ;

   -- octet
   declare
      Arg : Corba.Octet := Corba.Octet(5) ;
   begin
   Put_Line ("####### Test of Octet #######") ;
   Put_Line("I sent : " & Corba.Octet'Image(Arg)) ;
   Arg := EchoOctet(MyAll_Types, Arg) ;
   Put_Line("I received : " & Corba.Octet'Image(Arg)) ;
   Put_Line("") ;
   Put_Line("") ;
   end ;

   -- string
   declare
      Arg : Corba.String := Corba.To_Corba_String("Hello world");
   begin
   Put_Line ("####### Test of String #######") ;
   Put_Line("I sent : " & Corba.To_Standard_String(Arg)) ;
   Arg := EchoString(MyAll_Types, Arg) ;
   Put_Line("I received : " & Corba.To_Standard_String(Arg)) ;
   Put_Line("") ;
   Put_Line("") ;
   end ;



   -------------------
   -- complex types --
   -------------------

   -- simple exception
   declare
   begin
      Put_Line ("####### Test of exception #######") ;
      Put_Line ("I call simple_exception_test") ;
      Simple_Exception_Test(MyAll_Types);
   exception
      when Simple_Exception =>
         Put_Line ("A simple exception has just been catched !!!") ;
   end ;

   Put_Line ("") ;
   Put_Line ("") ;

   -- complexe exception
   declare
      Member : Complexe_Exception_Members ;
   begin
      Put_Line ("####### Test of exception #######") ;
      Put_Line ("I call complexe_exception_test") ;
      Complexe_Exception_Test(MyAll_Types);
   exception
      when E : Complexe_Exception =>
         Put_Line ("A complexe exception has just been catched !!!") ;
         Get_Members (E,Member) ;
         Put_Line ("It has a member whose value is : " &
                   Corba.Long'Image(Member.Excep)) ;
   end ;

   Put_Line ("") ;
   Put_Line ("") ;

   -- Union
   declare
      Ex,Ex2 : Example := (Switch => 2, Flags => True) ;
   begin
      Put_Line ("####### Test of union #######") ;
      Put ("I send the union ");
      Put (Ex) ;
      Put_Line ("") ;
      Ex2 := Echo1 (MyAll_Types,Ex) ;
      Put ("I received the union ") ;
      Put (Ex2) ;
      Put_Line ("") ;
   end ;

   Put_Line ("") ;
   Put_Line ("") ;

   -- struct
   declare
      Str,Str2 : Simple_Struct := (A => (0,1,2,3,4,5,6,7,8,9), B => 10) ;
   begin
      Put_Line ("####### Test of struct #######") ;
      Put ("I send the simple_struct ");
      Put (Str) ;
      Put_Line ("") ;
      str2 := echo2 (MyAll_Types,str) ;
      Put ("I received the simple_struct ");
      Put (Str2) ;
      Put_Line ("") ;
   end ;

   Put_Line ("") ;
   Put_Line ("") ;

   -- enum
   declare
      En,En2 : Color := Blue ;
   begin
      Put_Line ("####### Test of enum #######") ;
      Put_Line ("I send the color " & Color'Image(En)) ;
      En2 := Echo3 (MyAll_Types,En) ;
      Put_Line ("I received the color " & Color'Image(En2)) ;
   end ;

   Put_Line ("") ;
   Put_Line ("") ;

   -- unbounded strings
   declare
      Str,Str2 : U_String := U_String(Corba.To_Corba_String("Hello Adabroker !!!")) ;
   begin
      Put_Line ("####### Test of unbounded strings #######") ;
      Put_Line ("I send the unbounded string " &
                Corba.To_Standard_String(Corba.String(Str))) ;
      Str2 := Echo4 (MyAll_Types,Str) ;
      Put_Line ("I received the unbounded string " &
                Corba.To_Standard_String(Corba.String(Str2))) ;
   end ;

   Put_Line ("") ;
   Put_Line ("") ;

   -- unbounded sequences
   declare
      Seq,Seq2 : U_Sequence := U_Sequence (IDL_SEQUENCE_Short.Null_Sequence) ;
   begin
      Seq := Seq & 1 & 2 & 3 & 4 & 5 ;
      Put_Line ("####### Test of the unbounded sequences #######") ;
      Put ("I send the unbounded sequence") ;
      Put (Seq) ;
      Put_Line ("");
      Put ("I received the unbounded sequence") ;
      Seq2 := Echo6 (MyAll_Types,Seq) ;
      Put (Seq2) ;
      Put_Line("") ;
   end;

   Put_Line ("") ;
   Put_Line ("") ;

   -- bounded sequences
   declare
      Seq,Seq2 : B_Sequence := B_Sequence (IDL_SEQUENCE_Long_1.Null_Sequence) ;
   begin
      Seq := Seq & 1 & 2 & 3 & 4 & 5 ;
      Put_Line ("####### Test of the bounded sequences #######") ;
      Put ("I send the bounded sequence") ;
      Put (Seq) ;
      Put_Line("") ;
      Put ("I received the unbounded sequence") ;
      Seq2 := Echo7 (MyAll_Types,Seq) ;
      Put (Seq2) ;
      Put_Line("") ;
   end ;

   Put_Line ("");
   Put_Line ("");

   -- readonly attributes
   declare
   begin
      Put_Line ("####### Test of readonly attribute #######") ;
      Put_Line ("The value of this readonly attribute is " &
                Color'Image(Get_R_Attribute(MyAll_Types))) ;
   end ;

   Put_Line ("");
   Put_Line ("");


   -- attributes
   declare
      Ex,Ex2 : Example ;
      Ex3 : Example := (Switch => 2, Flags => True) ;
   begin
      Put_Line ("####### Test of attribute #######") ;
      Ex := Get_N_Attribute (MyAll_Types) ;
      Put ("The value of this attribute is ");
      Put (Ex) ;
      Put_Line ("");
      Put ("I can force it to ");
      Put (Ex3) ;
      Put_Line ("");
      Set_N_Attribute (MyAll_Types,Ex3) ;
      Ex2 := Get_N_Attribute (MyAll_Types) ;
      Put ("Now, the value of this attribute is ") ;
      Put(Ex2) ;
      Put_Line ("");
   end ;

   Put_Line ("");
   Put_Line ("");

   -- arrays
   declare
      Ex1 : Example := (Switch => 1, Counter => 19) ;
      Ex2 : Example := (Switch => 2, Flags => True) ;
      Ex3 : Example := (Switch => 3, Unknown => 25) ;
      Ar1 : All_Types.Line := (Ex1, Ex2, Ex3) ;
      Ar2 : All_Types.Line ;

      procedure Put (Ar : in All_Types.Line) is
      begin
         Put ("   ( ");
         Put (Ar(0));
         Put_Line (", ");
         Put ("     ");
         Put (Ar(1));
         Put_Line (", ");
         Put ("     ");
         Put (Ar(2));
         Put_Line (" )");
      end ;

   begin
      Put_Line ("####### Test of arrays #######") ;
      Put_Line ("I send the array :");
      Put (Ar1) ;

      Ar2 := Echo8 (MyAll_Types,Ar1) ;
      Put_Line ("I received the array :");
      Put (Ar2) ;
   end ;

   Put_Line ("");
   Put_Line ("");

   -- arrays (2)
   declare
      S1 : Simple_struct := (A => (0,1,2,3,4,5,6,7,8,9), B=> 23) ;
      S2 : Simple_struct := (A => (9,8,7,6,5,4,3,2,1,0), B=> 17) ;
      S3 : Simple_struct := (A => (0,1,2,3,4,5,6,7,8,9), B=> 23) ;
      S4 : Simple_struct := (A => (9,8,7,6,5,4,3,2,1,0), B=> 17) ;
      Ar1 : Square := ((S1, S2), (S3, S4)) ;
      Ar2 : Square ;

      procedure Put (Ar : in square) is
      begin
         Put ("   ( ");
         Put ("(0,0) => ");
         Put (Ar(0,0));
         Put_Line (", ");
         Put ("     (0,1) => ");
         Put (Ar(0,1));
         Put_Line (", ");
         Put ("     (1,0) => ");
         Put (Ar(1,0));
         Put_Line (", ");
         Put ("     (1,1) => ");
         Put (Ar(1,1));
         Put_Line (" )");
      end ;

   begin
      Put_Line ("####### Test of arrays (2) #######") ;
      Put_Line ("I send the array :");
      Put (Ar1) ;

      Ar2 := Echo9 (MyAll_Types,Ar1) ;
      Put_Line ("I received the array :");
      Put (Ar2) ;
   end ;

   Put_Line ("");
   Put_Line ("");

   -- arrays (3)
   declare
      S1 : Corba.string := Corba.To_Corba_String("case1") ;
      S2 : Corba.string := Corba.To_Corba_String("case2") ;
      S3 : Corba.string := Corba.To_Corba_String("case3") ;
      S4 : Corba.string := Corba.To_Corba_String("case4") ;
      S5 : Corba.string := Corba.To_Corba_String("case5") ;
      S6 : Corba.string := Corba.To_Corba_String("case6") ;
      S7 : Corba.string := Corba.To_Corba_String("case7") ;
      S8 : Corba.string := Corba.To_Corba_String("case8") ;
      Ar1 : cube := (((S1, S2), (S3, S4)), ((S5, S6), (S7, S8))) ;
      Ar2 : cube ;

      procedure Put (Ar : in cube) is
      begin
         Put ("   ( ");
         Put ("(0,0,0) => ");
         Put (Ar(0,0,0));
         Put_Line (", ");
         Put ("     (0,1,0) => ");
         Put (Ar(0,1,0));
         Put_Line (", ");
         Put ("     (1,0,0) => ");
         Put (Ar(1,0,0));
         Put_Line (", ");
         Put ("     (1,1,0) => ");
         Put (Ar(1,1,0));
         Put_Line (", ");
         Put ("     (0,0,1) => ");
         Put (Ar(0,0,1));
         Put_Line (", ");
         Put ("     (0,1,1) => ");
         Put (Ar(0,1,1));
         Put_Line (", ");
         Put ("     (1,0,1) => ");
         Put (Ar(1,0,1));
         Put_Line (", ");
         Put ("     (1,1,1) => ");
         Put (Ar(1,1,1));
         Put_Line (" )");
      end ;

   begin
      Put_Line ("####### Test of arrays (3) #######") ;
      Put_Line ("I send the array :");
      Put (Ar1) ;

      Ar2 := Echo10 (MyAll_Types,Ar1) ;
      Put_Line ("I received the array :");
      Put (Ar2) ;
   end ;

end Client ;
