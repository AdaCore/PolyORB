------------------------------------------------------------------------------
--                                                                          --
--                          DROOPI COMPONENTS                               --
--                                                                          --
--                             G I O P . G I O P 1.0                        --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
------------------------------------------------------------------------------


with Ada.Streams; use Ada.Streams;

with CORBA;

with Droopi.Opaque;
with Droopi.Buffers;
with Droopi.References;
with Droopi.Binding_Data;

package Droopi.Protocols.GIOP.GIOP_1_0 is

   No_Context : constant CORBA.Unsigned_Long := 0;

   procedure Marshall_GIOP_Header
     (Buffer       : access Buffer_Type;
      Message_Type : in Msg_Type;
      Message_Size : in Stream_Element_Offset);


   procedure Marshall_Request_Message
     (Buffer            : access Buffer_Type;
      Request_Id        : in CORBA.Unsigned_Long;
      Target_Profile    : in Binding_Data.Profile_Type;
      Response_Expected : in Boolean;
      Operation         : in Requests.Operation_Id);


   procedure Marshall_No_Exception
    (Buffer      : access Buffer_Type;
     Request_Id  : in CORBA.Unsigned_Long);


   procedure Marshall_Exception
    (Buffer           : access Buffer_Type;
     Request_Id       : in CORBA.Unsigned_Long;
     Exception_Type   : in Reply_Status_Exception;
     Occurence        : in CORBA.Exception_Occurrence);


   procedure Marshall_Location_Forward
    (Buffer           : access Buffer_Type;
     Request_Id       : in  CORBA.Unsigned_Long;
     Forward_Ref      : in  Droopi.References.Ref);


   ------------------------------------
   --- Unmarshalling receiving messages
   -------------------------------------

   procedure Unmarshall_Request_Message
     (Buffer            : access Buffer_Type;
      Request_Id        : out CORBA.Unsigned_Long;
      Response_Expected : out Boolean;
      Object_Key        : out Objects.Object_Id;
      Operation         : out CORBA.String);


   procedure Unmarshall_Reply_Message
      (Buffer       : access Buffer_Type;
       Request_Id   : out CORBA.Unsigned_Long;
       Reply_Status : out Reply_Status_Type);


private

   No_Context : constant CORBA.Unsigned_Long := 0;

   --  Version
   Major_Version : constant CORBA.Octet := 1;
   Minor_Version : constant CORBA.Octet := 0;

end Droopi.Protocols.GIOP.GIOP_1_0;
