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


package body Echo.impl is


   -- EchoString
   -------------
   function EchoString(Self : access Object;
                       Message : in Corba.String) return Corba.String is
      Result : Corba.String := in ;
   begin
      return Result ;
   end ;



End Echo.Impl ;



