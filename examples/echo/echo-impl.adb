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

with Omniropeandkey ;
with Netbufferedstream ; use Netbufferedstream ;
with Giop_S ; use Giop_S ;
with Giop ;

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

   -- Dipatch
   ----------
   procedure Dispatch (Self : in out Echo.Impl.Object ;
                       Orls : in out Giop_S.Object ;
                       Orl_Op : in Standard.String ;
                       Orl_Response_Expected : in Corba.Boolean ;
                       Returns : out Corba.Boolean ) is

   begin

      Output(Echo_Impl, "Echo.Impl.Dispatch") ;

      if Orl_Op = "echoString" then
         declare
            Mesg : Corba.String ;
            Result : Corba.String ;
            Mesg_Size : Corba.Unsigned_Long ;
         begin
            Unmarshall(Mesg, Orls) ;

            -- change state
            Request_Received(Orls) ;

            -- call the implementation
            Result := Echo.Impl.EchoString(Self'access, Mesg) ;

            -- computing the size of the replied message
            Mesg_Size := Giop_S.Reply_Header_Size (Orls);
            Mesg_Size := NetbufferedStream.Align_Size (Result,
                                                       Mesg_Size) ;

            -- Initialisation of the reply
            Giop_S.Initialize_Reply (Orls, Giop.NO_EXCEPTION, Mesg_Size) ;

            -- Marshall the arguments (here just a string)
            NetbufferedStream.Marshall (Result, Orls) ;

            -- inform the orb
            Giop_S.Reply_Completed (Orls) ;

            Returns := True ;
            return ;
         end;
      end if ;

      Returns := False ;
      return ;

   end ;

End Echo.Impl ;



