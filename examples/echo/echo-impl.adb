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
with Echo.Skeleton ;
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


   -----------------------------------------------------------
   -----------------------------------------------------------
   -- Initialize
   -------------
   procedure Initialize(Self : in out Object) is
   begin
      Omniobject.Init_Local_Object(Omniobject.Implemented_Object(Self),
                                   Repository_Id, Echo.Skeleton.Dispatch'Access) ;
   end Initialize ;


   -- Adjust
   ---------
   procedure Adjust(Self: in out Object) is
   begin
      Omniobject.Adjust(Omniobject.Implemented_Object(Self)) ;
   end ;


   -- Finalize
   -----------
   procedure Finalize(Self : in out Object) is
   begin
      Omniobject.Finalize(Omniobject.Implemented_Object(Self)) ;
   end ;



End Echo.Impl ;



