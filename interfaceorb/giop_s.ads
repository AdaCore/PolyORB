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



package Giop_S is

   type Object is limited private ;

   procedure RequestReceived (Self : in Object);
   -- wrapper around void GIOP_S::RequestReceived(CORBA::Boolean skip_msg)
   -- in giopServer.cc L 134
   --
   -- PROBLEME DE NOMBRE D'ARGUMENTS : vide = false ?
   --


   procedure InitialiseReply (Self : in Object,
                              Status : in GIOP::ReplyStatusType
                              MsgSize .... );
   -- wrapped around void GIOP_S::InitialiseReply(
   --                                    const GIOP::ReplyStatusType status,
   --                                    const size_t  msgsize)
   -- in giopServer.cc L 219


   procedure Put_Char_Array (Self : in out Object'class
                               ...
                            );
   -- wrapper around NetBufferedStream::put_char_array(const CORBA::Char* b, int size,
   --                             omni::alignment_t align,
   --                             CORBA::Boolean startMTU,
   --                             CORBA::Boolean at_most_once)
   -- in nbufferedStream.cc L 154


   procedure ReplyCompleted (Self : in Object);
   -- wrapper around void GIOP_S::ReplyCompleted()
   -- In giopServer.cc L 264


private



end Giop_S ;

