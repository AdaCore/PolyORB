with Text_Io ; use Text_Io ;
with Corba, Corba.Orb, Corba.Boa, Corba.Object ;
with Echo ;
with Cosnaming ; use Cosnaming ;
with Corba_Initialreferences ; use Corba_Initialreferences ;
with Cosnaming.Namingcontext ;
use Corba ;

procedure Client is

   Ior : Corba.String := Corba.To_Corba_String ("IOR:019f00402800000049444c3a6f6d672e6f72672f436f734e616d696e672f4e616d696e6743" &
                                                "6f6e746578743a312e3000010000000000000028000000010100000e0000003133372e3139342e" &
                                                "31342e333000d1070c00000036fb7c2715dbf21200000002");

   function Get_Object_Reference(Orb : in Corba.Orb.Object)
                                 return Corba.Object.Ref is
      Root_Context : Cosnaming.Namingcontext.Ref;
      Name : Cosnaming.Name ;
      Name_Service : Corba_Initialreferences.Ref ;
      Comp0, Comp1 : Cosnaming.Namecomponent ;
   begin
--      Root_Context := Corba.Orb.Resolve_Initial_References("NameService") ;
--
--      Comp0.Id := To_Corba_String("test") ;
--      Comp0.Kind := To_Corba_String("my_context") ;
--      Comp1.Id := To_Corba_String("Echo") ;
--      Comp1.Kind := To_Corba_String("Object") ;
--      -- Put Them in Name
--
--      return Resolve(Root_Context, Name) ;

      Corba.Orb.String_To_Object(Ior, Name_service) ;
      return Corba_Initialreferences.Get (Name_Service,
                                          ObjId (Corba.To_Corba_String("echo")));
   end ;


   Orb : Corba.Orb.Object := Corba.Orb.Orb_Init("omniORB2");
   Boa : Corba.Boa.Object := Corba.Orb.Boa_Init(Orb, "omniORB2_BOA") ;

   Myecho : Echo.Ref ;

   Sent_Msg : Corba.String ;
   Rcvd_Msg : Corba.String ;

begin

   Myecho := To_Ref(Get_Object_Reference(Orb)) ;

   -- checking if it worked
   if Echo.Is_Nil(myecho) then
      Put_Line("main : cannot invoke on a nil reference") ;
      return ;
   end if ;

   Sent_Msg := Corba.To_Corba_String("Hello World, Ada !") ;
   Rcvd_Msg := Echo.EchoString(Myecho, Sent_Msg) ;

   -- printing result
   Put_Line("I said : " & Corba.To_Standard_String(Sent_Msg) ) ;
   Put_Line("The object answered : " & Corba.To_Standard_String(Rcvd_Msg)) ;

end Client ;




