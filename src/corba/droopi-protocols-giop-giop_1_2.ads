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


package Droopi.Protocols.GIOP.GIOP_1_2 is


   procedure GIOP_Header_Marshall
     (Buffer       : access Buffers.Buffer_Type;
      Message_Type : in MsgType_1_2;
      Message_Size : in Stream_Element_Offset;
      Fragment_Next : in Boolean);


    procedure Request_Message_Marshall
     (Buffer            : access Buffers.Buffer_Type;
      Request_Id        : in CORBA.Unsigned_Long;
      Operation         : in Requests.Operation_Id;
      Addess_Type       : in AddressingDisposition;
      Target_Ref        : in TargetAddress;
      Sync_Type         : in CORBA.SyncScope);


    procedure No_Exception_Marshall
    (Buffer      : access Buffer_Type;
     Request_Id  : in CORBA.Unsigned_Long);


    procedure Exception_Marshall
    ( Buffer      : access Buffers.Buffer_Type;
      Requst_Id   : access CORBA.Unsigned_Long ;
      Reply_Type  : in ReplyStatusType range User_Exception..System_Exception;
      Occurence   : in CORBA.Exception_Occurrence);


    procedure Location_Forward_Marshall
    ( Buffer        :   access Buffers.Buffer_Type;
      Request_Id    :   access CORBA.Unsigned_Long;
      Reply_Type  : in ReplyStatusType range Location_Forward .. Location_Forward_Perm;
      Target_Ref  : in out Droopi.References);

    procedure Needs_Addressing_Mode_Marshall
    ( Buffer              : access Buffers.Buffer_Type;
      Request_Id          : in CORBA.Unsigned_Long;
      Address_Type        : in GIOP.Addressing_Disposition);

    procedure Cancel_Request_Marshall
    (Buffer     : access Buffers.Buffer_Type;
     Request_Id : in CORBA.Unsigned_Long);


    procedure Locate_Request_Marshall
    (Buffer            : access Buffer_Type;
     Request_Id        : in Corba.Unsigned-Long;
     Address_Type      : in Addressing_Disposition;
     Target_Ref        : in Target_Address);


    procedure Fragment_Marshall
    ( Buffer   : access Buffers.Buffer_Type;
      Req_Id   : in CORBA.Unsigned_Long);

    -------------------------------------
    --  Unmarshall procedures
    --------------------------------------

    procedure Request_Message_Unmarshall
     ( Buffer            : access Buffer_Type;
       Request_Id        : out Corba.Unisgned_Long;
       Response_Expected : out Boolean;
       Target_Ref        : out TargetAddress;
       Operation         : out Requests.Operation_Id;

    procedure Reply_Message_Unmarshall
      (Buffer       : access Buffer_Type;
       Request_Id   : out Corba.Unsigned_Long;
       Reply_Status : out Reply_Status_Type);

private


   Service_Context_List_1_2 : constant array (range 0 .. 9) of ServiceId
       := (Transaction_Service, CodeSets, ChainByPassCheck,
            ChainByPassInfo, LogicalThreadId, Bi_Dir_Iiop,
            SendingContextRunTime, Invocation_Policies,
            Forwarded_Identity, UnknownExceptionInfo);

   Major_Version : constant CORBA.Octet
     := 1;
   Minor_Version : constant CORBA.Octet
     := 2;

   Response_Flags: constant array(range 0..3) of CORBA.Octet:=
                   (0,16#1#, 16#2#, 16#3#);


end Droopi.Protocols.GIOP.GIOP_1_2;
