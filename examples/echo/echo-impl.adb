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

with Adabroker_Debug ; use Adabroker_Debug ;

package body Echo.impl is


   -- EchoString
   -------------
   function EchoString(Self : access Object;
                       Message : in Corba.String) return Corba.String is
      Result : Corba.String := Message ;
   begin
      return Result ;
   end ;

   -- Initialize
   -------------
   procedure Initialize(Self : in out Object) is
   begin
      Output(Echo_Impl,"Initializing Echo.Impl") ;
      Omniobject.Init_Local_object(Self, Repository_Id) ;
   end ;

End Echo.Impl ;



