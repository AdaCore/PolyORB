------------------------------------------------------------------------------
--                                                                          --
--                          DROOPI COMPONENTS                               --
--                                                                          --
--                             G I O P . G I O P 1.0                        --
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


package Droopi.Protocols.GIOP.GIOP_1_0  is


   No_Context : constant CORBA.Unsigned_Long:=0;

   procedure GIOP_Header_Marshall
     (Buffer       : access Buffer_Type;
      Message_Type : in MsgType;
      Message_Size : in Stream_Element_Offset);


   procedure Request_Message_Marshall
     (Buffer            : access Buffer_Type;
      Request_Id        : in CORBA.Unsigned_Long;
      Target_Profile    : in Binding_Data.Profile_Type;
      Response_Expected : in Boolean;
      Operation         : in Requests.Operation_Id);


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


   ------------------------------------
   --- Unmarshalling receiving messages
   -------------------------------------

   procedure Request_Message_Unmarshall
     (Buffer            : access Buffer_Type;
      Request_Id        : out Corba.Unisgned_Long;
      Response_Expected : out Boolean;
      Object_Key        : out Objects.Object_Id;
      Operation         : out Corba.String);


   procedure Reply_Message_Unmarshall
      (Buffer       : access Buffer_Type,
       Request_Id   : out Corba.Unsigned_Long,
       Reply_Status : out Reply_Status_Type);


private

   No_Context : constant CORBA.Unsigned_Long := 0;

    -- Version
   Major_Version : constant CORBA.Octet:= 1;
   Minor_Version : constant CORBA.Octet:= 0;

end Droopi.Protocols.GIOP.GIOP_1_0;
