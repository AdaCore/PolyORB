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


with Ada_Comand_Line ;
with Text_Io ; use Text_Io ;
with Corba, Corba.Orb, Corba.Boa ;
with Echo ;


procedure Client is

   IOR, Sent_Msg, Rcvd_msg : CORBA.String ;

   Obj : Corba.Object.Ref ;

   Obj_Echo : Echo.Ref ;


begin

   if Ada.Comand_Line.Argument_Count < 1 then
      Put_Line ("usage : client <IOR_string_from_server>") ;
      return ;
   end if ;

   -- transforms the Ada string into Corba.String
   IOR := Corba.String(Ada.Comand_Line.Argument(1)) ;

   -- ORB init
   Corba.Orb.Init("omniORB2") ;
   Put_Line("ORBinit done") ;

   -- BOA init
   Corba.Boa.Init("omniORB2_BOA") ;
   Put_Line("BOA init done") ;

   -- getting the Corba.Object
   Corba.Orb.String_To_Object(IOR, Obj) ;

   -- narrowing it into Echo
   Obj_Echo := Echo.To_Ref(Obj) ;

   -- checking if it worked
   if Corba.Object.Is_Nil(Obj_Echo) then
      Put_Line("cannot invoke on a nil reference") ;
      return ;
   end if ;

   -- sending message
   Sent_Msg := Corba.String("Hello World !") ;
   Rcvd_Msg := Echo.EchoString(Sent_Msg) ;

   -- printing result
   Put_Line("I said : " & Sent_Msg ) ;
   Put_Line("The object answered : " & Rcvd_Msg) ;

end Client ;



