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


   --  Declare a few constants for GIOP version 1.0



   -- Header Size of GIOP 1.0 Messages


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



private


   Service_Context_List_1_1 : constant array ( range 0 .. 1) of ServiceId
                              := (TransactionService, CodeSets);


end Droopi.Protocols.GIOP.GIOP_1_1;
