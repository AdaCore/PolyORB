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

with Droopi.Buffers;
with Droopi.References;
with Droopi.References.IOR;
with Droopi.Binding_Data;
with Droopi.Types;

package Droopi.Protocols.GIOP.GIOP_1_0 is

   pragma Elaborate_Body;

   procedure Marshall_GIOP_Header
     (Buffer       : access Buffers.Buffer_Type;
      Message_Type : in Msg_Type;
      Message_Size : in Stream_Element_Offset);
      --  Total message size (including GIOP header).

   procedure Marshall_Request_Message
     (Buffer            : access Buffers.Buffer_Type;
      Request_Id        : in Types.Unsigned_Long;
      Target_Profile    : in Binding_Data.Profile_Access;
      Response_Expected : in Boolean;
      Operation         : in Requests.Operation_Id);

   procedure Marshall_No_Exception
    (Buffer      : access Buffers.Buffer_Type;
     Request_Id  : in Types.Unsigned_Long);


   procedure Marshall_Exception
    (Buffer           : access Buffers.Buffer_Type;
     Request_Id       : in Types.Unsigned_Long;
     Exception_Type   : in Reply_Status_Type;
     Occurence        : in CORBA.Exception_Occurrence);


   procedure Marshall_Location_Forward
    (Buffer           : access Buffers.Buffer_Type;
     Request_Id       : in  Types.Unsigned_Long;
     Forward_Ref      : in  Droopi.References.IOR.IOR_Type);


   ------------------------------------
   --- Unmarshalling receiving messages
   -------------------------------------

   procedure Unmarshall_Request_Message
     (Buffer            : access Buffers.Buffer_Type;
      Request_Id        : out Types.Unsigned_Long;
      Response_Expected : out Boolean;
      Object_Key        : out Objects.Object_Id_Access;
      Operation         : out Types.String);


   procedure Unmarshall_Reply_Message
      (Buffer       : access Buffers.Buffer_Type;
       Request_Id   : out Types.Unsigned_Long;
       Reply_Status : out Reply_Status_Type);



private

   No_Context : constant Types.Unsigned_Long := 0;

   --  Version
   Major_Version : constant Types.Octet := 1;
   Minor_Version : constant Types.Octet := 0;

end Droopi.Protocols.GIOP.GIOP_1_0;
