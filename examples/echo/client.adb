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
with Text_IO ; use Text_IO ;
with Corba, Corba.Orb, Corba.Boa, Corba.Object ;
with Echo ;

with AdaBroker.Exceptions;

procedure Client is
   -- Initialisation of The ORB
   Orb : Corba.Orb.Object := Corba.Orb.Orb_Init("omniORB2");

   -- Initialisation of the BOA
--   Boa : Corba.Boa.Object := Corba.Orb.Boa_Init(Orb, "omniORB2_BOA") ;

   Sent_Msg, Rcvd_Msg, IOR : CORBA.String ;

   myecho : Echo.Ref ;

begin

   if Ada.Command_Line.Argument_Count < 1 then
      Put_Line ("usage : client <IOR_string_from_server>") ;
      return ;
   end if ;

   -- transforms the Ada string into Corba.String
   IOR := Corba.To_Corba_String(Ada.Command_Line.Argument(1)) ;

   -- getting the Corba.Object
   Corba.Orb.String_To_Object(IOR, myecho) ;

   -- checking if it worked
   if Echo.Is_Nil(myecho) then
      Put_Line("main : cannot invoke on a nil reference") ;
      return ;
   end if ;

   -- sending message
   Sent_Msg := Corba.To_Corba_String(Standard.String'("Hello Ada !")) ;
   Rcvd_Msg := Echo.EchoString(myecho, Sent_Msg) ;

   -- printing result
   Put_Line("I said : " & Corba.To_Standard_String(Sent_Msg) ) ;
   Put_Line("The object answered : " & Corba.To_Standard_String(Rcvd_Msg)) ;

end Client ;
