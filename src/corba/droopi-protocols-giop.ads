------------------------------------------------------------------------------
--                                                                          --
--                          DROOPI COMPONENTS                               --
--                                                                          --
--                         C O R B A. G I O P                               --
--                                                                          --
--                               S p e c                                    --
--                                                                          --
----
--                                                                          --
------------------------------------------------------------------------------


with Ada.Streams;   use Ada.Streams;

with CORBA;

with Droopi.Opaque;
with Droopi.Buffers;
with Droopi.Binding_Data;
with Droopi.References;
with Droopi.Protocols;

package CORBA.GIOP_Session is


   Message_Maximum_Size    : constant Integer;
   Endianess_Bit: constant Integer;
   Fragment_Bit: constant Integer;
   Byte_Order_Offset : constant Integer;

   type GIOP_Session is new Session with private;

   type GIOP_Protocol is new Protocol with private;

   type GIOP_Request is new Requests.Request  with private;

   type Sync_Scope is (NONE, WITH_TRANSPORT, WITH_SERVER, WITH_TARGET);

   type Bits_8 is CORBA.Octet;

   type TargetAddress(Address_Type: Integer) is
    record
     case Address_Type is
      when 0 =>
        Object_key: Objects.Object_Id;
      when 1 =>
        Profile: Binding_Data.Iiop.Iiop_Profile_Type;
      when 2 =>
        Ref    : References.ref;
     end case;
    end record;


   --  GIOP:: MsgType
   type Msg_Type is
     (Request,
      Reply,
      Cancel_Request,
      Locate_Request,
      Locate_Reply,
      Close_Connection,
      Message_Error,
      Fragment);


   --  GIOP::ReplyStatusType
   type Reply_Status_Type is
     (No_Exception,
      User_Exception,
      System_Exception,
      Location_Forward,
      Location_Forward_Perm,
      Needs_Addressing_Mode);

   --  GIOP::LocateStatusType
   type Locate_Status_Type is
     (Unknown_Object,
      Object_Here,
      Object_Forward,
      Object_Forward_Perm,
      Loc_System_Exception,
      Loc_Needs_Addressing_Mode);

   type Pending_Request is private;

   type Response_Sync(Version :  range 0 .. 1) is
    record
       case Version is
         when 0 =>
            Response_Expected : CORBA.Boolean;
         when 1 | 2 =>
            Sync_Type         : GIOP.SyncScope;
       end case;
    end record;

   type Send_Request_Result_Type is
     (Sr_No_Reply,
      Sr_Reply,
      Sr_User_Exception,
      Sr_Forward,
      Sr_Forward_Perm,
      Sr_Needs_Addressing_Mode
     );

   type Locate_Request_Result_Type is
     (Sr_Unknown_Object,
      Sr_Object_Here,
      Sr_Object_Forward,
      Sr_Object_Forward_Perm,
      Sr_Loc_System_Exception,
      Sr_Loc_Needs_Addressing_Mode
     );


   -- type GIOP_Version is private;

   package Octet_Sequences is new CORBA.Sequences.Unbounded(CORBA.Octet);
   subtype CORBA_Octet_Array is Octet_Sequences.Sequence.Element_Array;

   -- Define Types of Target Addresses used with Request Message


   type ServiceId is (
       Transaction_Service, CodeSets, ChainByPassCheck,
       ChainByPassInfo, LogicalThreadId, Bi_Dir_Iiop,
       SendingContextRunTime, Invocation_Policies,
       Forwarded_Identity, UnknownExceptionInfo
       );


  -----------------------------------------------------------
  -- Some common marshalling procedures for GIOP 1.0, 1.1, 1.2
  ------------------------------------------------------------

   procedure Exception_Marshall
    ( Buffer           : access Buffer_Type;
      Request_Id       : in CORBA.Unsigned_Long;
      Exception_Type   : in ReplyStatusType;
      Occurence        : in CORBA.Exception_Occurrence);


   procedure Location_Forward_Marshall
    ( Buffer           : access Buffer_Type;
      Request_Id       : in  CORBA.Unsigned_Long;
      Forward_Ref      : in  Droopi.References.Ref);


   procedure Cancel_Request_Marshall
     (Buffer           : access Buffer_Type;
      Request_Id       : in CORBA.Unsigned_Long);


   procedure Locate_Request_Marshall
     (Buffer           : access Buffer_Type;
      Request_Id       : in CORBA.Unsigned_Long;
      Profile_Ref      : in Binding_Data.Profile_Type );

   procedure Locate_Reply_Marshall
     (Buffer         : access Buffer_Type;
      Request_Id     : in CORBA.Unsigned_Long;
      Locate_Status  : in LocateReplyStatus);

   -----------------------------------
   -- Unmarshall
   ----------------------------------

   procedure Unmarshall_GIOP_Header
     (Ses                   : access GIOP_Session;
      Message_Type          : out MsgType;
      Message_Size          : out CORBA.Unsigned_Long;
      Fragment_Next         : out CORBA.Boolean;
      Success               : out Boolean);

   procedure Locate_Reply_Unmarshall
     (Buffer        : access Buffer_Type;
      Request_Id    : out Corba.Unsigned_Long;
      Locate_Status : out Locate_Status_Type);



  ---------------------------------------
  --- Marshalling switch  -----------
  --------------------------------------


   procedure Request_Message
    (Ses             : access GIOP_Session;
     Message_Size    : in Stream_Element_Offset;
     Target_Profile  : in Binding_Data.Profile_Type;
     Sync            : in Response_Sync;
     Fragment_Next   : out boolean )


    procedure Exception_Reply
    (Ses             : access GIOP_Session;
     Message_Size    : in Stream_Element_Offset ;
     Exception_Type  : in Reply_Status_Type range User_Exception..System_Exception;
     Occurence       : in CORBA.Exception_Occurrence)


    procedure Location_Forward_Reply
    (Ses             : access GIOP_Session;
     Message_Size    : in Stream_Element_Offset ;
     Exception_Type  : in Reply_Status_Type range Location_Forward .. Location_Forward_Perm;
     Forward_Ref     : in Droopi.References.Ref;
     Fragment_Next   : out Boolean);

    procedure Need_Addressing_Mode_Message
    (Ses             : access GIOP_Session;
     Message_Size    : in Stream_Element_Offset;
     Address_Type    : in Addressing_Disposition)

    procedure Cancel_Request_Message
    (Ses             : access GIOP_Session;
     Message_Size    : in Stream_Element_Offset)

    procedure Locate_Request_Message
    (Ses             : access GIOP_Session;
     Message_Size    : in Stream_Element_Offset ;
     Address_Type    : in Address_Disposition;
     Target_Ref      : in Target_Address;
     Fragment_Next   : out Boolean)

    procedure Locate_Reply_Message
    (Ses             : access GIOP_Session;
     Message_Size    : in Stream_Element_Offset;
     Locate_Status   : in Locate_Status_Type)


   -------------------------------------------
   -- Session procedures
   ------------------------------------------


   procedure Create
     (Proto   : access GIOP_Protocol;
      Session : out Filter_Access);

   procedure Connect (S : access GIOP_Session);

   procedure Invoke_Request (S : access GIOP_Session; R : Request);

   procedure Abort_Request (S : access GIOP_Session; R : Request);

   procedure Send_Reply (S : access GIOP_Session; R : Request);

   procedure Handle_Connect_Indication (S : access GIOP_Session);

   procedure Handle_Connect_Confirmation (S : access GIOP_Session);

   procedure Handle_Data_Indication (S : access GIOP_Session);

   procedure Handle_Disconnect (S : access GIOP_Session);



   ----------------------------------------
   ---  Pending requests primitives
   ---------------------------------------

   procedure Initialise_Request
     (Req     : access Request;
      Profile : in Iiop_Profile_type);


private

   type GIOP_Session is new Session with record
      Major_Version        : CORBA.Octet;
      Minor_Version        : CORBA.Octet;
      Buffer_Out           : access Buffers.Buffer_Type;
      Buffer_In            : Stream_Element_array;
      Role                 : Endpoint_Role;
      Target_Profile       : Binding_Data.Iiop.Iiop_Profile_Type;
      Object_Found         : Boolean := False;
      Nbr_Tries            : Natural := 0;
      Expect_Header        : Boolean := True;
   end record;

   type GIOP_Protocol is new Protocol with null record;

   type Pending_Request is record
     Req          : Requests.Request_Access;
     Request_Id   : Corba.Unsigned_Long := 0;
   end record;


   type Message_Param is record
     Mess_Type : Msg_Type;
     Mess_Size : CORBA.Unsigned_Long;
   end record;


   procedure Expect_Data
     (S             : access GIOP_Session;
      In_Buf        : Buffers.Buffer_Access;
      Expect_Max    : Ada.Streams.Stream_Element_Count);

   Message_Header_Size : constant :=12;

   Message_Body_Size : constant :=1000;

   Max_Data_Received : constant := 1024;

   Endianess_Bit: constant Integer:=1;

   Fragment_Bit: constant Integer:=2;

   Byte_Order_Offset : constant Integer:= 6;

   Max_Nb_Tries : constant Integer := 100;

end Corba.GIOP_Session;
