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

   -- simple types

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

   -- complex types


   -- Test of the simple exception
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

   --  Test of the complexe exception
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

   -- Test of Union
   declare
      Ex,Ex2 : Example := (Switch => 2, Flags => True) ;
      Tmp,Tmp2 : String (1..100) ;
   begin
      Put_Line ("####### Test of union #######") ;
      Tmp := "I send the union switch = " &
                Corba.Long'Image(Ex.Switch) &
                " and";
      case Ex.Switch is
         when 1 => Tmp := Tmp & " Counter = " & Corba.Long'Image(Ex.Counter) ;
         when 2 => Tmp := Tmp & " Flags = " & Corba.Boolean'Image(Ex.Flags) ;
         when others => Tmp := Tmp & " Unknown = " & Corba.Long'Image(Ex.Unknown) ;
      end case ;
      Put_Line(Tmp) ;
      Ex2 := Echo1 (MyAll_Types,Ex);
      Tmp2 := "I received the union switch = " &
                Corba.Long'Image(Ex2.Switch) &
                " and";
      case Ex.Switch is
         when 1 => Tmp2 := Tmp2 & " Counter = " & Corba.Long'Image(Ex2.Counter) ;
         when 2 => Tmp2 := Tmp2 & " Flags = " & Corba.Boolean'Image(Ex2.Flags) ;
         when others => Tmp2 := Tmp2 & " Unknown = " & Corba.Long'Image(Ex2.Unknown) ;
      end case ;
      Put_Line(Tmp2) ;
   end ;

   Put_Line ("") ;
   Put_Line ("") ;

   -- Test of struct
   declare
      Str,Str2 : Simple_Struct := (A => (0,1,2,3,4,5,6,7,8,9,10), B => 11) ;
      Tmp,Tmp2 : String (1..100) ;
   begin
      Put_Line ("####### Test of struct #######") ;
      Tmp := "I send the simple_struct a = (" & Corba.Long'Image(Str.A(0));
      for I in 1 .. 10 loop
         Tmp := Tmp & "," & Corba.Long'Image(Str.A(I)) ;
      end loop ;
      Tmp := Tmp & " and b = " & Corba.Long'Image(Str.B) ;
      Put_Line(Tmp) ;
      str2 := echo2 (MyAll_Types,str) ;
      Tmp2 := "I received the simple_struct a = (" & Corba.Long'Image(Str2.A(0)) ;
      for I in 1 .. 10 loop
         Tmp2 := Tmp2 & "," & Corba.Long'Image(Str2.A(I)) ;
      end loop ;
      Tmp2 := Tmp2 & " and b = " & Corba.Long'Image(Str2.B) ;
      Put_Line(Tmp2) ;
   end ;

   Put_Line ("") ;
   Put_Line ("") ;

   -- Test of enum
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

   -- Test of unbounded strings
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

   -- Test of unbounded sequences
   declare
      Seq,Seq2 : U_Sequence := U_Sequence (IDL_SEQUENCE_Short.Null_Sequence) ;
      Tmp,Tmp2 : String (1..39) ;
   begin
      Seq := Seq & 1 & 2 & 3 & 4 & 5 ;
      Put_Line ("####### Test of the unbounded sequences #######") ;
      Tmp := ("I send the unbounded sequence") ;
      for I in 1..Length(Seq) loop
         Tmp := Tmp & " " & Corba.Short'Image(Element_Of(Seq,I)) ;
      end loop ;
      Put_Line(Tmp) ;
      Seq2 := Echo6 (MyAll_Types,Seq) ;
      Tmp2 := ("I received the unbounded sequence") ;
      for I in 1..Length(Seq2) loop
         Tmp2 := Tmp2 & " " & Corba.Short'Image(Element_Of(Seq2,I)) ;
      end loop ;
      Put_Line(Tmp2) ;
   end;

   Put_Line ("") ;
   Put_Line ("") ;

   -- Test of bounded sequences
   declare
      Seq,Seq2 : B_Sequence := B_Sequence (IDL_SEQUENCE_Long_1.Null_Sequence) ;
      Tmp,Tmp2 : String (1..39) ;
   begin
      Seq := Seq & 1 & 2 & 3 & 4 & 5 ;
      Put_Line ("####### Test of the bounded sequences #######") ;
      Tmp := ("I send the bounded sequence") ;
      for I in 1..Length(Seq) loop
         Tmp := Tmp & " " & Corba.Long'Image(Element_Of(Seq,I)) ;
      end loop ;
      Put_Line(Tmp) ;
      Seq2 := Echo7 (MyAll_Types,Seq) ;
      Tmp2 := ("I received the unbounded sequence") ;
      for I in 1..Length(Seq2) loop
         Tmp2 := Tmp2 & " " & Corba.Long'Image(Element_Of(Seq2,I)) ;
      end loop ;
      Put_Line(Tmp2) ;
   end ;

   Put_Line ("");
   Put_Line ("");

   -- Test of readonly attributes
   declare
   begin
      Put_Line ("####### Test of readonly attribute #######") ;
      Put_Line ("The value of this readonly attribute is " &
                Color'Image(Get_R_Attribute(MyAll_Types))) ;
   end ;

   Put_Line ("");
   Put_Line ("");

   -- Test of readonly attributes
   declare
   begin
      Put_Line ("####### Test of readonly attribute #######") ;
      Put_Line ("The value of this readonly attribute is " &
                Color'Image(Get_R_Attribute(MyAll_Types))) ;
   end ;

   Put_Line ("");
   Put_Line ("");

   -- Test of attributes
   declare
      Ex,Ex2 : Example ;
      Ex3 : Example := (Switch => 2, Flags => True) ;
      Tmp,Tmp2 : String (1..39) ;
   begin
      Put_Line ("####### Test of attribute #######") ;
      Ex := Get_N_Attribute (MyAll_Types) ;
      Tmp := "The value of this attribute is switch = "
        & Corba.Long'Image(Ex.Switch) & " and" ;
      case Ex.Switch is
         when 1 => Tmp := Tmp & " Counter = " & Corba.Long'Image(Ex.Counter) ;
         when 2 => Tmp := Tmp & " Flags = " & Corba.Boolean'Image(Ex.Flags) ;
         when others => Tmp := Tmp & " Unknown = " & Corba.Long'Image(Ex.Unknown) ;
      end case ;
      Put_Line (Tmp) ;
      Put_Line ("I can force it to (Switch => 2, Flags => True)") ;
      Set_N_Attribute (MyAll_Types,Ex3) ;
      Ex2 := Get_N_Attribute (MyAll_Types) ;
      Tmp := "Now, the value of this attribute is switch = "
        & Corba.Long'Image(Ex2.Switch) & " and" ;
      case Ex.Switch is
         when 1 => Tmp2 := Tmp2 & " Counter = " & Corba.Long'Image(Ex2.Counter) ;
         when 2 => Tmp2 := Tmp2 & " Flags = " & Corba.Boolean'Image(Ex2.Flags) ;
         when others => Tmp2 := Tmp2 & " Unknown = " & Corba.Long'Image(Ex2.Unknown) ;
      end case ;
      Put_Line (Tmp2) ;
   end ;

end Client ;



