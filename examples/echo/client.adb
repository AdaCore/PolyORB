----------------------------------------------------------------------------
----                                                                    ----
----     This in an example which is hand-written                       ----
----     for the echo object (corresponds to eg2_clt.cc in omniORB      ----
----                                                                    ----
----                client                                              ----
----                                                                    ----
----                authors : Fabien Azavant, Sebastien Ponce           ----
----                                                                    ----
----------------------------------------------------------------------------


with Ada.Command_Line ;
with Text_Io ; use Text_Io ;
with Corba, Corba.Orb, Corba.Boa, Corba.Object ;
with Echo ;


procedure Client is

   Orb : Corba.Orb.Object ;
   Boa : Corba.Boa.Object ;

   Sent_Msg, Rcvd_Msg, IOR : CORBA.String ;

   myecho : Echo.Ref ;


begin

   Put_Line("main : Starting client") ;

   Orb := Corba.Orb.Orb_Init("omniORB2") ;
   Put_Line("main : ORB initialized") ;

   Boa := Corba.Orb.Boa_Init(Orb, "omniORB2_BOA") ;
   Put_Line("main : BOA initialized") ;




   if Ada.Command_Line.Argument_Count < 1 then
      Put_Line ("usage : client <IOR_string_from_server>") ;
      return ;
   end if ;

   -- transforms the Ada string into Corba.String
   IOR := Corba.To_Corba_String(Ada.Command_Line.Argument(1)) ;

   -- getting the Corba.Object
   Corba.Orb.String_To_Object(IOR, myecho) ;
   Put_Line("main : Got the Corba.Object") ;

   -- checking if it worked
   if Corba.Object.Is_Nil(myecho) then
      Put_Line("main : cannot invoke on a nil reference") ;
      return ;
   end if ;
   Put_Line("main : Ok : Corba.Object is not nil") ;

   Put_Line(Corba.To_Standard_String(Corba.Orb.Object_To_String(Myecho))) ;

   -- sending message
   Sent_Msg := Corba.To_Corba_String("Hello World !") ;
   Rcvd_Msg := Echo.EchoString(myecho, Sent_Msg) ;

   -- printing result
   Put_Line("I said : " & Corba.To_Standard_String(Sent_Msg) ) ;
   Put_Line("The object answered : " & Corba.To_Standard_String(Rcvd_Msg)) ;

end Client ;



