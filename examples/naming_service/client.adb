with Text_Io ; use Text_Io ;
with Corba, Corba.Orb, Corba.Boa, Corba.Object ;
with Echo ;
with Cosnaming ; use Cosnaming ;
with Corba_Initialreferences ; use Corba_Initialreferences ;
with Cosnaming.Namingcontext ;
use Corba ;

procedure Client is

   -- initialization of the ORB
   Orb : Corba.Orb.Object := Corba.Orb.Orb_Init("omniORB2");

   -- initialization of the BOA
   Boa : Corba.Boa.Object := Corba.Orb.Boa_Init(Orb, "omniORB2_BOA") ;

   -- IOR of the Naming service
   Ior : Corba.String := Corba.To_Corba_String
     ("IOR:019f00402800000049444c3a6f6d672e6f72672f436f734e616d696e672f4e616d6"
      & "96e67436f6e746578743a312e3000010000000000000028000000010100000e000000"
      & "3133372e3139342e31342e333000d1070c0000003701508f7454f45200000002") ;

   Root_context : Cosnaming.Namingcontext.Ref ;
   ObjName : Cosnaming.Name := Cosnaming.Name (Cosnaming.IDL_SEQUENCE_NameComponent.Null_Sequence) ;
   El : NameComponent ;

   Context : Cosnaming.Namingcontext.Ref ;
   Context_Name : Cosnaming.Name := Cosnaming.Name (Cosnaming.IDL_SEQUENCE_NameComponent.Null_Sequence) ;
   Context_El : Cosnaming.NameComponent ;

   Myecho : Echo.Ref ;

   Sent_Msg : Corba.String ;
   Rcvd_Msg : Corba.String ;

begin

   -- get the root_context of the name_service
   Corba.Orb.String_To_Object(Ior, Root_context) ;

   -- compute the name of the service
   El := (Id => IString(Corba.To_Corba_String(Standard.String'("echo"))),
          Kind => IString(Corba.To_Corba_String(Standard.String'("object")))) ;
   ObjName := ObjName & El ;

   -- compute the name of a new context
   Context_El := (Id => Cosnaming.IString(Corba.To_Corba_String(Standard.String'("champagne"))),
                  Kind => Cosnaming.IString(Corba.To_Corba_String(Standard.String'("context")))) ;
   Context_Name := Context_Name & Context_El ;

   begin
      -- build a new context
      Context := Cosnaming.Namingcontext.Bind_New_Context (Root_Context, Context_Name) ;
   exception
      when COSNAMING.NAMINGCONTEXT.ALREADYBOUND =>
         Cosnaming.Namingcontext.Rebind_Context (Root_Context,
                                                 Context_Name,
                                                 Context) ;
   end;

   -- get the object
   Myecho := Echo.To_Ref(Cosnaming.Namingcontext.Resolve(Context,ObjName)) ;

   if (Echo.Is_Nil(Myecho)) then
      Put_Line ("main : Unable to get an echo Object") ;
      return ;
   end if ;

   -- checking if it worked
   if Echo.Is_Nil(myecho) then
      Put_Line("main : cannot invoke on a nil reference") ;
      return ;
   end if ;

   Sent_Msg := Corba.To_Corba_String(Standard.String'("Hello World, Ada !")) ;
   Rcvd_Msg := Echo.EchoString(Myecho, Sent_Msg) ;

   -- printing result
   Put_Line("I said : " & Corba.To_Standard_String(Sent_Msg) ) ;
   Put_Line("The object answered : " & Corba.To_Standard_String(Rcvd_Msg)) ;

end Client ;




