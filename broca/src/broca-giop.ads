with CORBA;
with CORBA.Object;
with Broca.Object;
with Broca.Buffers;
with Broca.IOP;

package Broca.GIOP is
   --  Declare a few constants for GIOP version 1.0.

   --  GIOP::MsgType
   type MsgType is
     (Request,
      Reply,
      Cancel_Request,
      Locate_Request,
      Locate_Reply,
      Close_Connection,
      Message_Error,
      Fragment);

   --  GIOP::ReplyStatusType
   type ReplyStatusType is
     (No_Exception,
      User_Exception,
      System_Exception,
      Location_Forward);

   --  GIOP::LocateStatusType
   type LocateStatusType is
     (Unknown_Object,
      Object_Here,
      Object_Forward);

   --  Size (in bytes) of struct MessageHeader, major version 1.
   Message_Header_Size : constant := 12;

   No_Context : constant CORBA.Unsigned_Long := 0;

   procedure Compute_GIOP_Header_Size
     (Buffer : in out Buffers.Buffer_Descriptor);

   procedure Compute_New_Size
     (Buffer : in out Buffers.Buffer_Descriptor;
      Value  : in MsgType);

   procedure Compute_New_Size
     (Buffer : in out Buffers.Buffer_Descriptor;
      Value  : in ReplyStatusType);

   procedure Compute_New_Size
     (Buffer : in out Buffers.Buffer_Descriptor;
      Value  : in LocateStatusType);

   procedure Compute_New_Size
     (Buffer     : in out Buffers.Buffer_Descriptor;
      Request_Id : in CORBA.Unsigned_Long;
      Occurence  : in CORBA.Exception_Occurrence);

   procedure Compute_New_Size
     (Buffer     : in out Buffers.Buffer_Descriptor;
      Request_Id : in CORBA.Unsigned_Long;
      Reference  : in CORBA.Object.Ref);

   procedure Marshall_GIOP_Header
     (Buffer       : in out Buffers.Buffer_Descriptor;
      Message_Type : in MsgType);

   procedure Marshall
     (Buffer : in out Buffers.Buffer_Descriptor;
      Value  : in MsgType);

   procedure Marshall
     (Buffer : in out Buffers.Buffer_Descriptor;
      Value  : in ReplyStatusType);

   procedure Marshall
     (Buffer : in out Buffers.Buffer_Descriptor;
      Value  : in LocateStatusType);

   procedure Marshall
     (Buffer     : in out Buffers.Buffer_Descriptor;
      Request_Id : in CORBA.Unsigned_Long;
      Occurence  : in CORBA.Exception_Occurrence);

   procedure Marshall
     (Buffer     : in out Buffers.Buffer_Descriptor;
      Request_Id : in CORBA.Unsigned_Long;
      Reference  : in CORBA.Object.Ref);

   procedure Unmarshall
     (Buffer : in out Buffers.Buffer_Descriptor;
      Result : out MsgType);

   procedure Unmarshall
     (Buffer : in out Buffers.Buffer_Descriptor;
      Result : out ReplyStatusType);

   procedure Unmarshall
     (Buffer : in out Buffers.Buffer_Descriptor;
      Result : out LocateStatusType);

   procedure Unmarshall_GIOP_Header
     (Buffer       : in out Buffers.Buffer_Descriptor;
      Message_Type : out MsgType;
      Message_Size : out CORBA.Unsigned_Long);

   type Request_Handler is
      record
         Buffer     : Buffers.Buffer_Descriptor;
         Request_Id : CORBA.Unsigned_Long;
         Profile    : IOP.Profile_Ptr;
         Connection : IOP.Connection_Ptr;
         Nbr_Tries  : Natural := 0;
      end record;

   --  Send a request.
   procedure Send_Request_Size
     (Handler   : in out Request_Handler;
      Target    : in Object.Object_Ptr;
      Operation : in CORBA.Identifier);

   procedure Send_Request_Marshall
     (Handler          : in out Request_Handler;
      Reponse_Expected : in Boolean;
      Operation        : in CORBA.Identifier);

   type Send_Request_Result_Type is
     (Sr_No_Reply,
      Sr_Reply,
      Sr_User_Exception,
      Sr_Forward);

   procedure Send_Request_Send
     (Handler          : in out Request_Handler;
      Target           : in Object.Object_Ptr;
      Reponse_Expected : in Boolean;
      Result           : out Send_Request_Result_Type);

end Broca.GIOP;
