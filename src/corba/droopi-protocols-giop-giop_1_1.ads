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

with Droopi.Opaque;
with Droopi.Buffers;
with Droopi.References;
with Droopi.Binding_Data;


package Droopi.Protocols.GIOP.GIOP_1_1  is

   pragma Elaborate_Body;

   Service_Context_List_1_1 : constant array (range 0 .. 1) of ServiceId;
   type Service_Id_Array is array (Integer range <>) of ServiceId;
   Service_Context_List_1_1 : constant Service_Id_Array;

   procedure Marshall_GIOP_Header
     (Buffer        : access Buffers.Buffer_Type;
      Message_Type  : in Msg_Type;
      Message_Size  : in Stream_Element_Offset;
      Fragment_Next : in Boolean);


   procedure Marshall_Request_Message
     (Buffer                : access Buffers.Buffer_Type;
      Request_Id            : in CORBA.Unsigned_Long;
      Target_Profile        : in Binding_Data.Profile_Type;
      Response_Expected     : in Boolean;
      Operation             : in Requests.Operation_Id);

   procedure Marshall_Fragment
    ( Buffer      : access Buffer_Type;
      Request_Id  : in CORBA.Unsigned_Long);


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


   procedure Unmarshall_Request_Message
     ( Buffer            : access Buffer_Type;
       Request_Id        : out CORBA.Unisgned_Long;
       Response_Expected : out Boolean;
       Object_Key        : out Objects.Object_Id;
       Operation         : out CORBA.String;
       Principal         : out Stream_Element_Array);

    procedure Unmarshall_Reply_Message
      (Buffer       : access Buffer_Type;
       Request_Id   : out CORBA.Unsigned_Long;
       Reply_Status : out Reply_Status_Type);


private

   Service_Context_List_1_1 : constant Service_Id_Array
                := (0=> Transaction_Service, 1=> CodeSets);

   Major_Version : constant CORBA.Octet
     := 1;
   Minor_Version : constant CORBA.Octet
     := 1;

end Droopi.Protocols.GIOP.GIOP_1_1;
