------------------------------------------------------------------------------
--                                                                          --
--                          DROOPI COMPONENTS                               --
--                                                                          --
--                         C O R B A. G I O P                               --
--                                                                          --
--                               S p e c                                    --
--                                                                          --
------------------------------------------------------------------------------

with Ada.Streams;   use Ada.Streams;

with CORBA;
with CORBA.NVList;
with CORBA.Impl;

with Droopi.Opaque;
with Droopi.Buffers;
with Droopi.Binding_Data;
with Droopi.Binding_Data.IIOP;
with Droopi.References;
with Droopi.References.IOR;
with Droopi.Protocols;
with Droopi.Requests;
with Droopi.Objects;
with Droopi.ORB;
with Droopi.Log;

with Sequences.Unbounded;

package Droopi.Protocols.GIOP is


   use Droopi.Log;

   package Arg_Seq is new Sequences.Unbounded (CORBA.NamedValue);

   package L is new Droopi.Log.Facility_Log ("droopi.protocols.giop");
   procedure O (Message : in String; Level : Log_Level := Debug)
     renames L.Output;


   Message_Header_Size  : constant Stream_Element_Offset;
   Maximum_Message_Size : constant Stream_Element_Offset;
   Max_Data_Received    : constant Integer;
   Endianness_Bit       : constant Integer;
   Fragment_Bit         : constant Integer;
   Byte_Order_Offset    : constant Integer;
   Max_Nb_Tries         : constant Integer;


   Magic : constant Stream_Element_Array :=
     (Character'Pos ('G'),
      Character'Pos ('I'),
      Character'Pos ('O'),
      Character'Pos ('P'));


   type GIOP_Session is new Session with private;

   type GIOP_Protocol is new Protocol with private;

   type Sync_Scope is (NONE, WITH_TRANSPORT, WITH_SERVER, WITH_TARGET);

   type IOR_Addressing_Info is record
      Selected_Profile_Index : CORBA.Unsigned_Long;
      IOR                    : References.IOR.IOR_Type;
   end record;

   type IOR_Addressing_Info_Access is access all IOR_Addressing_Info;

   type Addressing_Disposition is (Key_Addr, Profile_Addr, Reference_Addr);

   type Version is (Ver0, Ver1, Ver2);

   type Target_Address (Address_Type : Addressing_Disposition) is record
      case Address_Type is
         when Key_Addr =>
            Object_Key : Objects.Object_Id_Access;
         when Profile_Addr  =>
            Profile : Binding_Data.Profile_Access;
         when Reference_Addr  =>
            Ref : IOR_Addressing_Info_Access;
      end case;
   end record;

   type Target_Address_Access is access all Target_Address;

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

  type Pending_Request is limited private;

   --  type Response_Sync(Version :  range 0 .. 1) is
   --  record
   --    case Version is
   --      when 0 =>
   --         Response_Expected : CORBA.Boolean;
   --      when 1 | 2 =>
   --         Sync_Type         : SyncScope;
   --    end case;
   --   end record;

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



   type ServiceId is
     (Transaction_Service,
      Code_Sets,
      Chain_By_Pass_Check,
      Chain_By_Pass_Info,
      Logical_Thread_Id,
      Bi_Dir_IIOP,
      Sending_Context_Run_Time,
      Invocation_Policies,
      Forwarded_Identity,
      Unknown_Exception_Info);


   AddressingDisposition_To_Unsigned_Long :
     constant array (Addressing_Disposition'Range) of CORBA.Unsigned_Long
     := ( Key_Addr => 0,
          Profile_Addr => 1,
          Reference_Addr => 2);


   Unsigned_Long_To_AddressingDisposition :
     constant array (CORBA.Unsigned_Long range 0 ..2) of Addressing_Disposition
     := (  0 => Key_Addr,
           1 => Profile_Addr,
           2 => Reference_Addr);


   Version_To_Unsigned_Long :
     constant array (Version'Range) of CORBA.Unsigned_Long
     := ( Ver0  => 0,
          Ver1  => 1,
          Ver2  => 2);


   Unsigned_Long_To_Version :
     constant array (CORBA.Unsigned_Long range 0 ..2) of Version
     := (  0 => Ver0,
           1 => Ver1,
           2 => Ver2);


   -----------------------------------------
   -- Some utilities functions Marshalling
   ----------------------------------------

   --  Specs

   procedure Marshall
     (Buffer : access Buffers.Buffer_Type;
      Value  : in Msg_Type);

   procedure Marshall
     (Buffer : access Buffers.Buffer_Type;
      Value  : in Reply_Status_Type);

   procedure Marshall
     (Buffer : access Buffers.Buffer_Type;
      Value  : in Locate_Status_Type);

   function Unmarshall
     (Buffer : access Buffers.Buffer_Type)
     return Msg_Type;

   function Unmarshall
     (Buffer : access Buffers.Buffer_Type)
     return Reply_Status_Type;

   function Unmarshall
     (Buffer : access Buffers. Buffer_Type)
     return Locate_Status_Type;

   procedure Marshall
     (Buffer : access Buffers.Buffer_Type;
      Value  : in Version);

   function Unmarshall
     (Buffer : access Buffers.Buffer_Type)
    return Version;





   -----------------------------------------------------------
   --  Some common marshalling procedures for GIOP 1.0, 1.1, 1.2
   ------------------------------------------------------------

  -- procedure Marshall_Exception
  --  (Buffer           : access Buffers.Buffer_Type;
  --    Request_Id       : in CORBA.Unsigned_Long;
  --    Exception_Type   : in Reply_Status_Type;
  --    Occurence        : in CORBA.Exception_Occurrence);


  -- procedure Marshall_Location_Forward
  --   (Buffer           : access Buffers.Buffer_Type;
  --    Request_Id       : in  CORBA.Unsigned_Long;
  --    Forward_Ref      : in  Droopi.References.Ref);


   procedure Marshall_Cancel_Request
     (Buffer           : access Buffers.Buffer_Type;
      Request_Id       : in CORBA.Unsigned_Long);

   procedure Marshall_Locate_Request
     (Buffer           : access Buffers.Buffer_Type;
      Request_Id       : in CORBA.Unsigned_Long;
      Object_Key       : in Objects.Object_Id_Access);

   procedure Marshall_Locate_Reply
     (Buffer         : access Buffers.Buffer_Type;
      Request_Id     : in CORBA.Unsigned_Long;
      Locate_Status  : in Locate_Status_Type);

   -----------------------------------
   --  Unmarshall
   ----------------------------------

   procedure Unmarshall_GIOP_Header
     (Ses                   : access GIOP_Session;
      Message_Type          : out Msg_Type;
      Message_Size          : out CORBA.Unsigned_Long;
      Fragment_Next         : out CORBA.Boolean;
      Success               : out Boolean);

   procedure Unmarshall_Locate_Reply
     (Buffer        : access Buffers.Buffer_Type;
      Request_Id    : out Corba.Unsigned_Long;
      Locate_Status : out Locate_Status_Type);



   ---------------------------------------
   ---  Marshalling switch  -----------
   --------------------------------------

   procedure Request_Message
     (Ses               : access GIOP_Session;
      Response_Expected : in Boolean;
      Fragment_Next     : out boolean);

   procedure No_Exception_Reply
     (Ses           : access GIOP_Session;
      Request_Id    : in CORBA.Unsigned_Long;
      Fragment_Next : out boolean);


    procedure Exception_Reply
     (Ses             : access GIOP_Session;
      Exception_Type  : in Reply_Status_Type;
      Occurence       : in CORBA.Exception_Occurrence;
      Fragment_Next   : out Boolean);


   procedure Location_Forward_Reply
     (Ses             : access GIOP_Session;
      Forward_Ref     : in Droopi.References.IOR.Ior_Type;
      Fragment_Next   : out Boolean);

   procedure Need_Addressing_Mode_Message
     (Ses             : access GIOP_Session;
      Address_Type    : in Addressing_Disposition);

   procedure Cancel_Request_Message
     (Ses             : access GIOP_Session);


   procedure Locate_Request_Message
     (Ses             : access GIOP_Session;
      Object_key      : in Objects.Object_Id_Access;
      Fragment_Next   : out Boolean);


   procedure Locate_Reply_Message
     (Ses             : access GIOP_Session;
      Locate_Status   : in Locate_Status_Type);


   -------------------------------------------
   --  Session procedures
   ------------------------------------------


   procedure Create
     (Proto   : access GIOP_Protocol;
      Session : out Filter_Access);


   procedure Invoke_Request (S : access GIOP_Session; R : Requests.Request);

   procedure Abort_Request (S : access GIOP_Session; R :  Requests.Request);

   procedure Send_Reply (S : access GIOP_Session; R :  Requests.Request);

   procedure Handle_Connect_Indication (S : access GIOP_Session);

   procedure Handle_Connect_Confirmation (S : access GIOP_Session);

   procedure Handle_Data_Indication (S : access GIOP_Session);

   procedure Handle_Disconnect (S : access GIOP_Session);



   ----------------------------------------
   ---  Pending requests primitives
   ---------------------------------------




   GIOP_Error : exception;

private

   type GIOP_Session is new Session with record
      Major_Version        : Version := Ver1;
      Minor_Version        : Version;
      Buffer_Out           : Buffers.Buffer_Access;
      Buffer_In            : Buffers.Buffer_Access;
      Role                 : ORB.Endpoint_Role;
      Object_Found         : Boolean := False;
      Nbr_Tries            : Natural := 0;
      Expect_Header        : Boolean := True;
      Mess_Type_Received   : Msg_Type;
   end record;

   type GIOP_Protocol is new Protocol with null record;

   type Pending_Request is record
      Req             : Requests.Request_Access;
      Request_Id      : CORBA.Unsigned_Long := 0;
      Target_Profile  : Binding_Data.Profile_Access;
   end record;

   Message_Header_Size : constant Stream_Element_Offset := 12;

   Maximum_Message_Size : constant Stream_Element_Offset := 1000;

   Max_Data_Received : constant Integer := 1024;

   Endianness_Bit : constant Integer := 1;

   Fragment_Bit : constant Integer := 2;

   Byte_Order_Offset : constant Integer := 6;

   Max_Nb_Tries : constant Integer := 100;

end Droopi.Protocols.GIOP;
