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


   --  Declare a few constants for GIOP version 1.0


   -- Header Size of GIOP 1.0 Messages



   No_Context : constant CORBA.Unsigned_Long;

   function To_Principal
     (S : String)
     return Principal;

   --  Convert string S into a Principal containing the characters that
   --  constitute S, followed by an ASCII NUL.

   --  Size (in bytes) of struct MessageHeader, major version 1.


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


--   procedure Exception_Marshall
--    ( Buffer           : access Buffer_Type;
--      Request_Id       : in CORBA.Unsigned_Long;
--      Exception_Type   : in ReplyStatusType;
--      Occurence        : in CORBA.Exception_Occurrence);


--   procedure Location_Forward_Marshall
--    (
--      Buffer           : access Buffer_Type;
--      Request_Id       : in  CORBA.Unsigned_Long;
--      Forward_Ref      : in  Droopi.References.Ref);


--   procedure Cancel_Request_Marshall
--     (Buffer           : access Buffer_Type;
--      Request_Id       : in CORBA.Unsigned_Long);


--   procedure Locate_Request_Marshall
--     (Buffer           : access Buffer_Type;
--      Request_Id       : in CORBA.Unsigned_Long;
--      Profile_Ref      : in Binding_Data.Profile_Type );

--   procedure Locate_Reply_Marshall
--   (
--     Buffer         : access Buffer_Type;
--     Request_Id     : in CORBA.Unsigned_Long;
--     Locate_Status  : in LocateReplyStatus);


private


   No_Context : constant CORBA.Unsigned_Long := 0;


end Droopi.Protocols.GIOP.GIOP_1_0;
