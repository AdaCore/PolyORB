with Text_Io ; use Text_Io ;
with Corba, Corba.Orb, Corba.Boa, Corba.Object ;
with Echo ;
with Cosnaming ; use Cosnaming ;
with Corba_Initialreferences ; use Corba_Initialreferences ;
with Cosnaming.Namingcontext ;
use Corba ;

procedure Client is

   Orb : Corba.Orb.Object := Corba.Orb.Orb_Init("omniORB2");
   Boa : Corba.Boa.Object := Corba.Orb.Boa_Init(Orb, "omniORB2_BOA") ;

   Ior : Corba.String := Corba.To_Corba_String
     ("IOR:019f00402800000049444c3a6f6d672e6f72672f436f734e616d696e672f4e616d6"
      & "96e67436f6e746578743a312e3000010000000000000028000000010100000e000000"
      & "3133372e3139342e31342e333000d1070c0000003701508f7454f45200000002") ;


   Root_context : Cosnaming.Namingcontext.Ref ;
   ObjName : Cosnaming.Name := Cosnaming.Name (Cosnaming.IDL_SEQUENCE_NameComponent.Null_Sequence) ;
   El : NameComponent ;

   Myecho : Echo.Ref ;

   Sent_Msg : Corba.String ;
   Rcvd_Msg : Corba.String ;

begin

Put_Line ("test1");
   -- get the root_context of the name_service
   Corba.Orb.String_To_Object(Ior, Root_context) ;

Put_Line ("test2");
   -- compute the name of the service
   El := (Id => IString(Corba.To_Corba_String("echo")),
          Kind => IString(Corba.To_Corba_String("object"))) ;
Put_Line ("test2bis");
   ObjName := ObjName & El ;

   -- get the object
Put_Line ("test3");
   Myecho := Echo.To_Ref(Cosnaming.Namingcontext.Resolve(Root_Context,ObjName)) ;
Put_Line (Corba.To_Standard_String(Corba.Orb.Object_To_String(Myecho))) ;

   -- checking if it worked
Put_Line ("test4");
   if Echo.Is_Nil(myecho) then
      Put_Line("main : cannot invoke on a nil reference") ;
      return ;
   end if ;

Put_Line ("test5");
   Sent_Msg := Corba.To_Corba_String("Hello World, Ada !") ;
Put_Line ("test6");
   Rcvd_Msg := Echo.EchoString(Myecho, Sent_Msg) ;

   -- printing result
Put_Line ("test7");
   Put_Line("I said : " & Corba.To_Standard_String(Sent_Msg) ) ;
Put_Line ("test8");
   Put_Line("The object answered : " & Corba.To_Standard_String(Rcvd_Msg)) ;

end Client ;




