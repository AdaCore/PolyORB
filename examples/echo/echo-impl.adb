----------------------------------------------------------------------------
----                                                                    ----
----     This in an example which is hand-written                       ----
----     for the echo object                                            ----
----                                                                    ----
----                package echo_impl                                   ----
----                                                                    ----
----                authors : Fabien Azavant, Sebastien Ponce           ----
----                                                                    ----
----------------------------------------------------------------------------

with Corba ; use Corba ;

with Adabroker_Debug ; use Adabroker_Debug ;

package body Echo.impl is


   -- EchoString
   -------------
   function EchoString(Self : access Object;
                       Message : in Corba.String) return Corba.String is
      Result : Corba.String := Message ;
   begin
      Output(Echo_Impl,"*** Je suis dans echoString") ;
      return Result ;
   end ;


   -- EchoLong
   -------------
   function EchoLong(Self : access Object;
                     Message : in Corba.Long) return Corba.Long is
      Result : Corba.Long := Message ;
   begin
      Output(Echo_Impl,"*** Je suis dans echoLong") ;
      return Result ;
   end ;



End Echo.Impl ;



