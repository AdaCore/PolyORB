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



package Giop_C is

   type Object new NetBufferedStream.Object with null record;

   procedure Init (Self : in out Object, R Rope.Object) ;
   -- wrapper around GIOP_C::GIOP_C(Rope *r)
   -- in giopClient.cc L54

   procedure Put_Char_Array (Self: in Object;
                               B: in Corba.String;
                               Size: in Integer;
                               Align: in Alignment_T := Omni.ALIGN_1;
                               StartMTU: in Corba.Boolean := False;
                               At_Most_One: in Corba.Boolean := False);
   -- wrapper around NetBufferedStream::put_char_array(const CORBA::Char* b,
   --                             int size,
   --                             omni::alignment_t align,
   --                             CORBA::Boolean startMTU,
   --                             CORBA::Boolean at_most_once)
   -- in nbufferedStream.cc L 154

   procedure Get_Char_Array (Self : in Object,
                               B: in Corba.String;
                               Size: in Integer;
                               Align: in Alignment_T := Omni.ALIGN_1;
                               StartMTU: in Corba.Boolean := False);
   -- wrapper around void NetBufferedStream::get_char_array(CORBA::Char* b,
   --                                               int size,
   --                                               omni::alignment_t align,
   --                                               CORBA::Boolean startMTU)
   -- in nbufferedStream.cc L 121

   function RequestHeaderSize (Objkeysize : in Size_T,
                              Opnamesize : in Size_T) return Size_T;
   -- wrapper around   static size_t RequestHeaderSize(const size_t objkeysize,
   --                                                  const size_t opnamesize);
   -- in giopDriver.h L 220

   procedure InitialiseRequest (Self : in OmniORB::ObjectKey,
                                 Objkey : in Size_T,
                                 Objkeysize : in Size_T,
                                 Opname : in CORBA::STRING,
                                 OpnameSize : in Size_T,
                                 MsgSize : in Size_T,
                                 Oneway : in CORBA::Boolean);
   -- wrapper arround void GIOP_C::InitialiseRequest(const void *objkey,
   --                     const size_t         objkeysize,
   --                     const char          *opname,
   --                     const size_t         opnamesize,
   --                     const size_t         msgsize,
   --                     const CORBA::Boolean oneway)
   -- in giopClient.cc L 119

   function ReceiveReply (Self : in Object) return ReplyStatusType;
   -- wrapper around GIOP::ReplyStatusType GIOP_C::ReceiveReply()
   -- in giopClient L174

   procedure RequestCompleted (Self : in Object, Skip_Msg : in CORBA::Boolean);
   -- wrapper around void GIOP_C::RequestCompleted(CORBA::Boolean skip_msg)
   -- in giopClient L 274


private



end Giop_C ;

