----------------------------------------------------------------------------
----                                                                    ----
----     This in an example which is hand-written                       ----
----     for the echo object                                            ----
----                                                                    ----
----                package echo_skeletons                              ----
----                                                                    ----
----                authors : Fabien Azavant, Sebastien Ponce           ----
----                                                                    ----
----------------------------------------------------------------------------

with Omniropeandkey ;
with Netbufferedstream ; use Netbufferedstream ;
with Giop_S ; use Giop_S ;
with Giop ;
with Corba ;
use type Corba.Unsigned_Long ;

with Adabroker_Debug ; use Adabroker_Debug ;

package body Echo.Skeleton is


   -- Initialize
   -------------
   procedure Initialize(Self : in out Object) is
   begin
      Omniobject.Init_Local_Object(Omniobject.Implemented_Object(Self),
                                   Repository_Id) ;
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



   -- Dipatch
   ----------
   procedure Dispatch (Self : access Object ;
                       Orls : in out Giop_S.Object ;
                       Orl_Op : in Standard.String ;
                       Orl_Response_Expected : in Corba.Boolean ;
                       Returns : out Corba.Boolean ) is
   begin
      if Orl_Op = "echoString" then
         declare
            Mesg : Corba.String ;
            Result : Corba.String ;
            Mesg_Size : Corba.Unsigned_Long ;
         begin

            Output(Echo_Skeleton, "Echo.Skeleton.Dispatch : echoString") ;

            Unmarshall(Mesg, Orls) ;

            -- change state
            Request_Received(Orls) ;

            -- call the implementation
            Result := EchoString(Object_Ptr(Self), Mesg) ;

            -- computing the size of the replied message
            Mesg_Size := Giop_S.Reply_Header_Size (Orls);
            Mesg_Size := Align_Size (Result, Mesg_Size) ;

            -- Initialisation of the reply
            Giop_S.Initialize_Reply (Orls, Giop.NO_EXCEPTION, Mesg_Size) ;

            -- Marshall the arguments (here just a string)
            Marshall (Result, Orls) ;

            -- inform the orb
            Giop_S.Reply_Completed (Orls) ;

            Returns := True ;
            return ;
         end;
      end if ;

      if Orl_Op = "echoLong" then
         declare
            Mesg : Corba.Long ;
            Result : Corba.Long ;
            Mesg_Size : Corba.Unsigned_Long ;
         begin

            Output(Echo_Skeleton, "Echo.Skeleton.Dispatch : echoLong") ;

            Unmarshall(Mesg, Orls) ;

            Output(Echo_Skeleton,
                   "Echo.Skeleton.Dispatch : got the long : "
                   & Integer'Image(Integer(Mesg))) ;

            -- change state
            Request_Received(Orls) ;

            Output(Echo_Skeleton,
                   "Request_Received ok !") ;

            -- call the implementation
            Result := EchoLong(Object_Ptr(Self), Mesg) ;

            Output(Echo_Skeleton, "Got the result : "
                   & Integer'Image(Integer(Result))) ;

            -- computing the size of the replied message
            Mesg_Size := Giop_S.Reply_Header_Size (Orls);
            Output(Echo_Skeleton,"reply header size : "
                   & Integer'Image(Integer(Mesg_Size))) ;

            Mesg_Size := Align_Size (Result, Mesg_Size) ;
            Output(Echo_Skeleton,"align size : "
                   & Integer'Image(Integer(Mesg_Size))) ;

            -- Initialisation of the reply
            Giop_S.Initialize_Reply (Orls, Giop.NO_EXCEPTION, Mesg_Size) ;
            Output(Echo_Skeleton,"reply initialized") ;

            -- Marshall the arguments
            Marshall (Result, Orls) ;
            Output(Echo_Skeleton, "result marshalled") ;

            -- inform the orb
            Giop_S.Reply_Completed (Orls) ;
            Output(Echo_Skeleton, "reply completed") ;

            Returns := True ;
            return ;
         end;
      end if ;

      Returns := False ;
      return ;

   end ;

end Echo.Skeleton ;
