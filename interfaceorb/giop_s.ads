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

   type Object is tagged limited private ;

   procedure RequestReceived (Self : in Object);
   -- wrapper around void GIOP_S::RequestReceived(CORBA::Boolean skip_msg)
   -- in giopServer.cc L 134
   --
   -- PROBLEME DE NOMBRE D'ARGUMENTS : vide = false ?
   --


   procedure InitialiseReply (Self : in Object ;
                              Status : in GIOP.ReplyStatusType ;
                              MsgSize : Integer );
   -- wrapped around void GIOP_S::InitialiseReply(
   --                                    const GIOP::ReplyStatusType status,
   --                                    const size_t  msgsize)
   -- in giopServer.cc L 219


   procedure Put_Char_Array (Self : in out Object'Class ;
                             B : Corba.Char ;
                             Size : Integer ;
                             Align : Omni.Alignment_T ;
                             StartMTU : Corba.Boolean ;
                             At_Most_Once : Corba.boolean
                            );
   -- wrapper around NetBufferedStream::put_char_array(const CORBA::Char* b,
   --                             int size,
   --                             omni::alignment_t align,
   --                             CORBA::Boolean startMTU,
   --                             CORBA::Boolean at_most_once)
   -- in nbufferedStream.cc L 154


   procedure ReplyCompleted (Self : in Object'Class);
   -- wrapper around void GIOP_S::ReplyCompleted()
   -- In giopServer.cc L 264


private

   type Object is tagged limited null record ;

end Giop_S ;

