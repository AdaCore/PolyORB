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

   Orb_ptr : Corba.Orb.Object_ptr ;
   Boa_ptr : Corba.Boa.Object_ptr ;

   Sent_Msg, Rcvd_Msg, IOR : CORBA.String ;

   myecho : Echo.Ref ;


begin

   Put_Line("Starting client") ;

   Orb_ptr := Corba.Orb.Orb_Init("omniORB2") ;
   Put_Line("ORB initialized") ;

   Boa_ptr := Corba.Orb.Boa_Init(Orb_Ptr, "omniORB2_BOA") ;
   Put_Line("BOA initialized") ;




   if Ada.Command_Line.Argument_Count < 1 then
      Put_Line ("usage : client <IOR_string_from_server>") ;
      return ;
   end if ;

   -- transforms the Ada string into Corba.String
   IOR := Corba.To_Corba_String(Ada.Command_Line.Argument(1)) ;

   -- getting the Corba.Object
   Corba.Orb.String_To_Object(IOR, myecho) ;

   -- checking if it worked
   if Corba.Object.Is_Nil(myecho) then
      Put_Line("cannot invoke on a nil reference") ;
      return ;
   end if ;

   -- sending message
   Sent_Msg := Corba.To_Corba_String("Hello World !") ;
   Rcvd_Msg := Echo.EchoString(myecho, Sent_Msg) ;

   -- printing result
   Put_Line("I said : " & Corba.To_Standard_String(Sent_Msg) ) ;
   Put_Line("The object answered : " & Corba.To_Standard_String(Rcvd_Msg)) ;

end Client ;



