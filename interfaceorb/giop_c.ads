-----------------------------------------------------------------------
----                                                               ----
----                  AdaBroker                                    ----
----                                                               ----
----                  package giop_c                               ----
----                                                               ----
----   authors : Sebastien Ponce, Fabien Azavant                   ----
----   date    :                                                   ----
----                                                               ----
----                                                               ----
-----------------------------------------------------------------------

with Corba, Omni, OmniORB, Giop, Rope, Netbufferedstream ;

package Giop_C is

   type Object is new NetBufferedStream.Object with null record;

   procedure Init (Self : in out Object ;
                   R : in Rope.Object) ;
   -- wrapper around GIOP_C::GIOP_C(Rope *r)
   -- in giopClient.cc L54

   procedure Put_Char_Array (Self: in Object ;
                               B: in Corba.String ;
                               Size: in Integer ;
                               Align: in Omni.Alignment_T := Omni.ALIGN_1 ;
                               StartMTU: in Corba.Boolean := False ;
                               At_Most_One: in Corba.Boolean := False ) ;
   -- wrapper around NetBufferedStream::put_char_array(const CORBA::Char* b,
   --                             int size,
   --                             omni::alignment_t align,
   --                             CORBA::Boolean startMTU,
   --                             CORBA::Boolean at_most_once)
   -- in nbufferedStream.cc L 154

   procedure Get_Char_Array (Self : in Object ;
                               B : in Corba.String ;
                               Size : in Integer ;
                               Align : in Omni.Alignment_T := Omni.ALIGN_1 ;
                               StartMTU : in Corba.Boolean := False) ;
   -- wrapper around void NetBufferedStream::get_char_array(CORBA::Char* b,
   --                                               int size,
   --                                               omni::alignment_t align,
   --                                               CORBA::Boolean startMTU)
   -- in nbufferedStream.cc L 121

   function RequestHeaderSize (Objkeysize : in Integer ;
                              Opnamesize : in Integer ) return Integer ;
   -- wrapper around   static size_t RequestHeaderSize(const size_t objkeysize,
   --                                                  const size_t opnamesize);
   -- in giopDriver.h L 220

   procedure InitialiseRequest (Self : in OmniORB.ObjectKey ;
                                 Objkey : in Integer ;
                                 Objkeysize : in Integer ;
                                 Opname : in CORBA.STRING ;
                                 OpnameSize : in Integer ;
                                 MsgSize : in Integer ;
                                 Oneway : in CORBA.Boolean) ;
   -- wrapper arround void GIOP_C::InitialiseRequest(const void *objkey,
   --                     const size_t         objkeysize,
   --                     const char          *opname,
   --                     const size_t         opnamesize,
   --                     const size_t         msgsize,
   --                     const CORBA::Boolean oneway)
   -- in giopClient.cc L 119

   function ReceiveReply (Self : in Object) return Giop.ReplyStatusType ;
   -- wrapper around GIOP::ReplyStatusType GIOP_C::ReceiveReply()
   -- in giopClient L174

   procedure RequestCompleted (Self : in Object ;
                               Skip_Msg : in CORBA.Boolean) ;
   -- wrapper around void GIOP_C::RequestCompleted(CORBA::Boolean skip_msg)
   -- in giopClient L 274


private



end Giop_C ;

