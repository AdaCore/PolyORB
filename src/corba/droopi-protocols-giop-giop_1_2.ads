------------------------------------------------------------------------------
--                                                                          --
--                          ADABROKER COMPONENTS                            --
--                                                                          --
--                       D R O O P I . G I O P. GIOP 1.2                    --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                                                                          --
--                                                                          --
------------------------------------------------------------------------------


with Ada.Streams; use Ada.Streams;

with CORBA;
with CORBA.Object;
with CORBA.AbstractBase;

with Droopi.Opaque;
with Droopi.Buffers;
with Droopi.Binding_Data;

with Droopi.Protocols.GIOP;


package Droopi.Protocols.GIOP.GIOP_1_2 is

   pragma Elaborate_Body;

   procedure Marshall_GIOP_Header
     (Buffer       : access Buffers.Buffer_Type;
      Message_Type : in MsgType;
      Message_Size : in Stream_Element_Offset;
      Fragment_Next : in Boolean);


    procedure Marshall_Request_Message
     (Buffer             : access Buffers.Buffer_Type;
      Request_Id         : in CORBA.Unsigned_Long;
      Operation          : in Requests.Operation_Id;
      Target_Ref         : in Target_Address;
      Sync_Type          : in CORBA.SyncScope);


    procedure Marshall_No_Exception
    (Buffer      : access Buffers.Buffer_Type;
     Request_Id  : in CORBA.Unsigned_Long);


    procedure Marshall_Exception
    ( Buffer      : access Buffers.Buffer_Type;
      Request_Id   : access CORBA.Unsigned_Long ;
      Reply_Type  : in Reply_Status_Type;
      Occurence   : in CORBA.Exception_Occurrence);


    procedure Marshall_Location_Forward
    ( Buffer        :   access Buffers.Buffer_Type;
      Request_Id    :   access CORBA.Unsigned_Long;
      Reply_Type  : in Reply_Status_Type;
      Target_Ref  : in out Droopi.References.Ref);

    procedure Marshall_Needs_Addressing_Mode
    ( Buffer              : access Buffers.Buffer_Type;
      Request_Id          : in CORBA.Unsigned_Long;
      Address_Type        : in Addressing_Disposition);

    procedure Marshall_Cancel_Request
    (Buffer     : access Buffers.Buffer_Type;
     Request_Id : in CORBA.Unsigned_Long);


    procedure Marshall_Locate_Request
    (Buffer            : access Buffers.Buffer_Type;
     Request_Id        : in CORBA.Unsigned-Long;
     Target_Ref        : in Target_Address);


    procedure Marshall_Fragment
    ( Buffer   : access Buffers.Buffer_Type;
      Req_Id   : in CORBA.Unsigned_Long);

    -------------------------------------
    --  Unmarshall procedures
    --------------------------------------

    procedure Unmarshall_Request_Message
     ( Buffer            : access Buffer_Type;
       Request_Id        : out CORBA.Unsigned_Long;
       Response_Expected : out Boolean;
       Target_Ref        : out Target_Address;
       Operation         : out Requests.Operation_Id);

    procedure Unmarshall_Reply_Message
      (Buffer       : access Buffer_Type;
       Request_Id   : out CORBA.Unsigned_Long;
       Reply_Status : out Reply_Status_Type);

private


   Service_Context_List_1_2 : constant array (Integer range 0 .. 9) of ServiceId
       := (Transaction_Service, CodeSets, ChainByPassCheck,
            ChainByPassInfo, LogicalThreadId, Bi_Dir_Iiop,
            SendingContextRunTime, Invocation_Policies,
            Forwarded_Identity, UnknownExceptionInfo);

   Major_Version : constant CORBA.Octet
     := 1;
   Minor_Version : constant CORBA.Octet
     := 2;

   Response_Flags: constant array(Integer range 0..3) of CORBA.Octet:=
                   (0,16#1#, 16#2#, 16#3#);


end Droopi.Protocols.GIOP.GIOP_1_2;
