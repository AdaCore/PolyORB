------------------------------------------------------------------------------
--                                                                          --
--                          PolyORB COMPONENTS                               --
--                                                                          --
--                           C O R B A . G I O P                            --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
----                                                                        --
--                                                                          --
------------------------------------------------------------------------------

--  $Id$

with Ada.Streams; use Ada.Streams;

with CORBA;

with PolyORB.Buffers;
with PolyORB.References;
with PolyORB.References.IOR;
with PolyORB.Binding_Data;
with PolyORB.Types;

package PolyORB.Protocols.GIOP.GIOP_1_1  is

   pragma Elaborate_Body;

   type Service_Id_Array is array (Integer range <>) of ServiceId;
   Service_Context_List_1_1 : constant Service_Id_Array;

   procedure Marshall_GIOP_Header
     (Buffer        : access Buffers.Buffer_Type;
      Message_Type  : in Msg_Type;
      Message_Size  : in Stream_Element_Offset;
      --  Total message size (including GIOP header).
      Fragment_Next : in Boolean);


   procedure Marshall_Request_Message
     (Buffer                : access Buffers.Buffer_Type;
      Request_Id            : in Types.Unsigned_Long;
      Target_Profile        : in Binding_Data.Profile_Access;
      Response_Expected     : in Boolean;
      Operation             : in Requests.Operation_Id);


   procedure Marshall_Fragment
    (Buffer      : access Buffers.Buffer_Type;
     Request_Id  : in Types.Unsigned_Long);


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
     Forward_Ref      : in  PolyORB.References.IOR.IOR_Type);


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

   --  Explicit bounds are required in the nominal subtype
   --  in order to comply with Ravenscar restriction
   --  No_Implicit_Heap_Allocation.

   Service_Context_List_1_1 : constant Service_Id_Array (0 .. 1)
     := (0 => Transaction_Service,
         1 => Code_Sets);

   Major_Version : constant Types.Octet
     := 1;
   Minor_Version : constant Types.Octet
     := 1;

end PolyORB.Protocols.GIOP.GIOP_1_1;
