with CORBA;
with CORBA.Object;
with Broca.Object;
with Broca.Types; use Broca.Types;

package Broca.Giop is
   --  Declare a few constants for GIOP version 1.0.

   --  GIOP::MsgType
   Request : constant CORBA.Unsigned_Long := 0;
   Reply : constant CORBA.Unsigned_Long := 1;
   Cancel_Request : constant CORBA.Unsigned_Long := 2;
   Locate_Request : constant CORBA.Unsigned_Long := 3;
   Locate_Reply : constant CORBA.Unsigned_Long := 4;
   Close_Connection : constant CORBA.Unsigned_Long := 5;
   Message_Error : constant CORBA.Unsigned_Long := 6;
   Fragment : constant CORBA.Unsigned_Long := 7;

   --  GIOP::ReplyStatusType
   No_Exception : constant CORBA.Unsigned_Long := 0;
   User_Exception : constant CORBA.Unsigned_Long := 1;
   System_Exception : constant CORBA.Unsigned_Long := 2;
   Location_Forward : constant CORBA.Unsigned_Long := 3;

   --  GIOP::LocateStatusType
   Unknown_Object : constant CORBA.Unsigned_Long := 0;
   Object_Here : constant CORBA.Unsigned_Long := 1;
   Object_Forward : constant CORBA.Unsigned_Long := 2;

   --  Size (in bytes) of struct MessageHeader, major version 1.
   Message_Header_Size : constant := 12;

   Magic : constant Buffer_Type (0 .. 3) :=
     (Character'Pos ('G'),
      Character'Pos ('I'),
      Character'Pos ('O'),
      Character'Pos ('P'));

   --  Reset Stream (set STREAM.POS to 0) and fill it with a message header,
   --  version 1.0
   --  STREAM must be at least Message_Header_Size length.
   --  Machine byte order is used.
   procedure Create_Giop_Header
     (Stream : in out Buffer_Descriptor;
      Message_Type : CORBA.Unsigned_Long;
      Message_Size : CORBA.Unsigned_Long);

   procedure Create_Reply_System_Exception
     (Stream : in out Buffer_Descriptor;
      Request_Id : CORBA.Unsigned_Long;
      Occurence : CORBA.Exception_Occurrence);

   procedure Create_Reply_Location_Forward
     (Stream : in out Buffer_Descriptor;
      Request_Id : CORBA.Unsigned_Long;
      Reference : CORBA.Object.Ref);
   type Request_Handler is
      record
         Buffer : Buffer_Descriptor;
         Request_Id : CORBA.Unsigned_Long;
         Profile : Broca.Object.Profile_Acc;
         Connection : Broca.Object.Connection_Acc;
         Nbr_Tries : Natural := 0;
      end record;

   --  Send a request.
   procedure Send_Request_Size (Handler : in out Request_Handler;
                                Object : Broca.Object.Object_Acc;
                                Operation : CORBA.Identifier);

   procedure Send_Request_Marshall (Handler : in out Request_Handler;
                                    Reponse_Expected : Boolean;
                                    Operation : CORBA.Identifier);

   type Send_Request_Result_Type is
      (Sr_No_Reply, Sr_Reply, Sr_User_Exception, Sr_Forward);

   procedure Send_Request_Send (Handler : in out Request_Handler;
                                Object : Broca.Object.Object_Acc;
                                Reponse_Expected : Boolean;
                                Res : out Send_Request_Result_Type);

end Broca.Giop;
