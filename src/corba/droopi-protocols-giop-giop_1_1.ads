------------------------------------------------------------------------------
--                                                                          --
--                          DROOPI COMPONENTS                               --
--                                                                          --
--                           C O R B A . G I O P                            --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
----                                                                        --
--                                                                          --
------------------------------------------------------------------------------


with Ada.Streams; use Ada.Streams;

with CORBA;
with CORBA.GIOP;

with Droopi.Opaque;
with Droopi.Buffers;
with Droopi.References
with Droopi.Binding_Data;


package Droopi.Protocols.GIOP.GIOP_1_1  is


   Service_Context_List_1_1 : constant array ( range 0 .. 1) of ServiceId;

   procedure GIOP_Header_Marshall
     (Buffer        : access Buffers.Buffer_Type;
      Message_Type  : in MsgType;
      Message_Size  : in Stream_Element_Offset;
      Fragment_Next : in Boolean);


   procedure Request_Message_Marshall
     (Buffer                : access Buffers.Buffer_Type;
      Request_Id            : in CORBA.Unsigned_Long;
      Target_Profile        : in Binding_Data.Profile_Type;
      Response_Expected     : in Boolean;
      Operation             : in Requests.Operation_Id);

   procedure Fragment_Marshall
    ( Buffer      : access Buffer_Type;
      Request_Id  : in CORBA.Unsigned_Long);


   procedure No_Exception_Marshall
    (Buffer      : access Buffer_Type;
     Request_Id  : in CORBA.Unsigned_Long)


   procedure Exception_Marshall
    (Buffer           : access Buffer_Type;
     Request_Id       : in CORBA.Unsigned_Long;
     Exception_Type   : in ReplyStatusType;
     Occurence        : in CORBA.Exception_Occurrence);


   procedure Location_Forward_Marshall
    (Buffer           : access Buffer_Type;
     Request_Id       : in  CORBA.Unsigned_Long;
    Forward_Ref      : in  Droopi.References.Ref);


   procedure Request_Message_Unmarshall
     ( Buffer            : access Buffer_Type;
       Request_Id        : out Corba.Unisgned_Long;
       Response_Expected : out Boolean;
       Object_Key        : out Objects.Object_Id;
       Operation         : out Corba.String;
       Principal         : out Stream_Element_Array);

    procedure Reply_Message_Unmarshall
      (Buffer       : access Buffer_Type;
       Request_Id   : out Corba.Unsigned_Long;
       Reply_Status : out Reply_Status_Type);


private

   Service_Context_List_1_1 : constant array ( range 0 .. 1) of ServiceId
                              := (TransactionService, CodeSets);

   Major_Version : constant CORBA.Octet
     := 1;
   Minor_Version : constant CORBA.Octet
     := 1;

end Droopi.Protocols.GIOP.GIOP_1_1;
