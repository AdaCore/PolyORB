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

   type Object is new NetBufferedStream.Object with private ;

   procedure Init (Self : in out Object ;
                   R : in Rope.Object) ;
   -- wrapper around GIOP_C::GIOP_C(Rope *r)
   -- in giopClient.cc L54



   function Request_Header_Size (Objkeysize : in Corba.Unsigned_long ;
                                 Opnamesize : in Integer )
                                 return Corba.Unsigned_long ;
   -- wrapper around   static size_t RequestHeaderSize(const size_t objkeysize,
   --                                                  const size_t opnamesize);
   -- in giopDriver.h L 220

   procedure Initialize_Request (Self : in Object ;
                                 Objkey : in Corba.Octet ;
                                 Objkeysize : in Corba.Unsigned_Long ;
                                 Opname : in CORBA.STRING ;
                                 MsgSize : in Corba.Unsigned_Long ;
                                 Oneway : in CORBA.Boolean) ;
   -- wrapper arround void GIOP_C::InitialiseRequest(const void *objkey,
   --                     const size_t         objkeysize,
   --                     const char          *opname,
   --                     const size_t         opnamesize,
   --                     const size_t         msgsize,
   --                     const CORBA::Boolean oneway)
   -- in giopClient.cc L 119

   function Receive_Reply (Self : in Object)
                           return Giop.ReplyStatusType ;
   -- wrapper around GIOP::ReplyStatusType GIOP_C::ReceiveReply()
   -- in giopClient L174

   procedure Request_Completed (Self : in Object ;
                               Skip_Msg : in CORBA.Boolean := False) ;
   -- wrapper around void GIOP_C::RequestCompleted(CORBA::Boolean skip_msg)
   -- in giopClient L 274


private

   type Object is new NetBufferedStream.Object with null record ;


end Giop_C ;

