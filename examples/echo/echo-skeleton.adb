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
with Echo.Impl ;
with Adabroker_Debug ; use Adabroker_Debug ;

package body Echo.Skeleton is




   -- Dipatch
   ----------
   procedure Dispatch (Myself : Omniobject.Implemented_Object_Ptr ;
                       Orls : in out Giop_S.Object ;
                       Orl_Op : in Standard.String ;
                       Orl_Response_Expected : in Corba.Boolean ;
                       Returns : out Corba.Boolean ) is
      Self : Echo.Impl.Object_Ptr := Echo.Impl.Object_Ptr(Myself) ;
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
            Result := Echo.Impl.EchoString(Self, Mesg) ;

            -- computing the size of the replied message
            Mesg_Size := Giop_S.Reply_Header_Size;
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
            Result := Echo.Impl.EchoLong(Self, Mesg) ;

            Output(Echo_Skeleton, "Got the result : "
                   & Integer'Image(Integer(Result))) ;

            -- computing the size of the replied message
            Mesg_Size := Giop_S.Reply_Header_Size;
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
