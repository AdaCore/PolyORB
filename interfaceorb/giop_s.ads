-----------------------------------------------------------------------
----                                                               ----
----                  AdaBroker                                    ----
----                                                               ----
----                  package giop_s                               ----
----                                                               ----
----   authors : Sebastien Ponce, Fabien Azavant                   ----
----   date    :                                                   ----
----                                                               ----
----                                                               ----
-----------------------------------------------------------------------

with Corba, Giop, Omni ;

package Giop_S is

   type Object is new Netbufferedstream.Object ;

   procedure Request_Received (Self : in Object);
   -- wrapper around void GIOP_S::RequestReceived(CORBA::Boolean skip_msg)
   -- in giopServer.cc L 134
   --
   -- PROBLEME DE NOMBRE D'ARGUMENTS : vide = false ?
   --


   procedure Initialize_Reply (Self : in Object ;
                              Status : in GIOP.ReplyStatusType ;
                              MsgSize : Integer );
   -- wrapped around void GIOP_S::InitialiseReply(
   --                                    const GIOP::ReplyStatusType status,
   --                                    const size_t  msgsize)
   -- in giopServer.cc L 219


   procedure Reply_Completed (Self : in Object);
   -- wrapper around void GIOP_S::ReplyCompleted()
   -- In giopServer.cc L 264


private

   type Object is new NetBufferedStream.Object with record
      CPP_Object : System.Address ;
   end ;


end Giop_S ;

