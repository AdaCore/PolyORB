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
--  For Exception_Occurrence.

with Sequences.Unbounded;

with Droopi.Buffers;
with Droopi.Binding_Data;
with Droopi.References;
with Droopi.References.IOR;
with Droopi.Requests;
with Droopi.Objects;
with Droopi.ORB;
with Droopi.Storage_Pools;
with Droopi.Types;
with Droopi.Representations.CDR;

package Droopi.Protocols.GIOP is

   --  Body requires child units GIOP_<version>:
   --  no elab control pragmas.

   use Droopi.Binding_Data;
   use ORB;

   GIOP_Error : exception;

   --    package Arg_Seq is new Sequences.Unbounded (Any.NamedValue);

   Message_Header_Size  : constant Stream_Element_Offset;
   Maximum_Message_Size : constant Stream_Element_Offset;
   Byte_Order_Offset    : constant Stream_Element_Offset;
   Max_Data_Received    : constant Integer;

   Max_Nb_Tries         : constant Integer;

   type GIOP_Session is new Session with private;
   type GIOP_Protocol is new Protocol with private;

   type Sync_Scope is (NONE, WITH_TRANSPORT, WITH_SERVER, WITH_TARGET);

   type IOR_Addressing_Info is record
      Selected_Profile_Index : Types.Unsigned_Long;
      IOR                    : References.IOR.IOR_Type;
   end record;

   type IOR_Addressing_Info_Access is access all IOR_Addressing_Info;
   for IOR_Addressing_Info_Access'Storage_Pool
     use Droopi.Storage_Pools.Debug_Pool;

   type Addressing_Disposition is (Key_Addr, Profile_Addr, Reference_Addr);

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
   for Target_Address_Access'Storage_Pool
     use Droopi.Storage_Pools.Debug_Pool;

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

   procedure Initialize;
   procedure Finalize;

   -------------------------
   -- Marshalling helpers --
   -------------------------

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

   ----------------------------------------------------------
   -- Common marshalling procedures for GIOP 1.0, 1.1, 1.2 --
   ----------------------------------------------------------

   --  procedure Marshall_Exception
   --   (Buffer           : access Buffers.Buffer_Type;
   --    Request_Id       : in Types.Unsigned_Long;
   --    Exception_Type   : in Reply_Status_Type;
   --    Occurence        : in Types.Exception_Occurrence);


   --  procedure Marshall_Location_Forward
   --   (Buffer           : access Buffers.Buffer_Type;
   --    Request_Id       : in  Types.Unsigned_Long;
   --    Forward_Ref      : in  Droopi.References.Ref);


   procedure Marshall_Cancel_Request
     (Buffer           : access Buffers.Buffer_Type;
      Request_Id       : in Types.Unsigned_Long);

   procedure Marshall_Locate_Request
     (Buffer           : access Buffers.Buffer_Type;
      Request_Id       : in Types.Unsigned_Long;
      Object_Key       : in Objects.Object_Id_Access);

   procedure Marshall_Locate_Reply
     (Buffer         : access Buffers.Buffer_Type;
      Request_Id     : in Types.Unsigned_Long;
      Locate_Status  : in Locate_Status_Type);

   ----------------
   -- Unmarshall --
   ----------------

   procedure Unmarshall_GIOP_Header
     (Ses                   : access GIOP_Session;
      Message_Type          : out Msg_Type;
      Message_Size          : out Types.Unsigned_Long;
      Fragment_Next         : out Types.Boolean;
      Success               : out Boolean);

   procedure Unmarshall_Locate_Reply
     (Buffer        : access Buffers.Buffer_Type;
      Request_Id    : out Types.Unsigned_Long;
      Locate_Status : out Locate_Status_Type);

   ------------------------
   -- Marshalling switch --
   ------------------------

   procedure Request_Message
     (Ses               : access GIOP_Session;
      Pend_Req      : access Pending_Request;
      Response_Expected : in Boolean;
      Fragment_Next     : out Boolean);

   procedure No_Exception_Reply
     (Ses           : access GIOP_Session;
      Request       :        Requests.Request_Access;
      Fragment_Next :    out Boolean);

   procedure Exception_Reply
     (Ses             : access GIOP_Session;
      Request         :  Requests.Request_Access;
      Exception_Type  : in Reply_Status_Type;
      Occurence       : in CORBA.Exception_Occurrence;
      Fragment_Next   : out Boolean);

   procedure Location_Forward_Reply
     (Ses             : access GIOP_Session;
      Request         :  Requests.Request_Access;
      Forward_Ref     : in Droopi.References.IOR.IOR_Type;
      Fragment_Next   : out Boolean);

   procedure Need_Addressing_Mode_Message
     (Ses             : access GIOP_Session;
      Request         : Requests.Request_Access;
      Address_Type    : in Addressing_Disposition);

   procedure Cancel_Request_Message
     (Ses             : access GIOP_Session;
      Request         : Requests.Request_Access);


   procedure Locate_Request_Message
     (Ses             : access GIOP_Session;
      Request         : Requests.Request_Access;
      Object_Key      : in Objects.Object_Id_Access;
      Fragment_Next   : out Boolean);


   procedure Locate_Reply_Message
     (Ses             : access GIOP_Session;
      Request         : Requests.Request_Access;
      Locate_Status   : in Locate_Status_Type);

   -------------------
   -- Store_Profile --
   -------------------

   procedure Store_Profile
    (Ses     : access GIOP_Session;
     Profile : Profile_Access);
   --  XXX What does this procedure do?
   --  Why does it need to be visible?

   -------------------
   -- Store_Request --
   -------------------

   procedure Store_Request
     (Ses     : access GIOP_Session;
      R       :        Requests.Request_Access;
      Profile :        Profile_Access;
      Pending :    out Pending_Request);
   --  XXX What does this procedure do?
   --  Why does it need to be visible?

   -----------------
   -- Set_Version --
   -----------------

   procedure Set_Version
     (S             : access GIOP_Session;
      Major_Version :        Types.Octet;
      Minor_Version :        Types.Octet);
   --  Set the version of the protocol to be used
   --  on a newly-created GIOP session.

   ------------------------
   -- Session primitives --
   ------------------------

   procedure Create
     (Proto   : access GIOP_Protocol;
      Session : out Filter_Access);

   procedure Invoke_Request (S : access GIOP_Session;
                 R : Requests.Request_Access);

   procedure Abort_Request (S : access GIOP_Session;
                 R : Requests.Request_Access);

   procedure Send_Reply (S : access GIOP_Session;
                 R : Requests.Request_Access);

   procedure Handle_Connect_Indication (S : access GIOP_Session);

   procedure Handle_Connect_Confirmation (S : access GIOP_Session);

   procedure Handle_Data_Indication (S : access GIOP_Session);

   procedure Handle_Disconnect (S : access GIOP_Session);

   ----------------------------------
   -- Utility function for testing --
   ----------------------------------

   procedure To_Buffer
     (S   : access GIOP_Session;
      Octets : access Representations.CDR.Encapsulation);

private

   --  Explicit bounds are required in the nominal subtype
   --  in order to comply with Ravenscar restriction
   --  No_Implicit_Heap_Allocation.

   Magic : constant Stream_Element_Array (1 .. 4)
     := (Character'Pos ('G'),
         Character'Pos ('I'),
         Character'Pos ('O'),
         Character'Pos ('P'));

   type Pending_Request is
     record
       Req             : Requests.Request_Access;
       Target_Profile  : Binding_Data.Profile_Access;
     end record;

   package Pend_Req_Seq is new Sequences.Unbounded (Pending_Request);

   package Req_Seq is new Sequences.Unbounded (Requests.Request_Access);

   type GIOP_State is
     (Expect_Header,
      --  Waiting for a new message header.

      Expect_Body,
      --  A message header has been received, waiting for
      --  the corresponding message body.

      Arguments_Ready
      --  A Request message has been received, and the arguments
      --  for the request are ready for unmarshalling at the input
      --  buffer's current position.
      );

   type GIOP_Session is new Session with record
      Major_Version        : Types.Octet := 1;
      Minor_Version        : Types.Octet := 2;
      --  By default, we implement GIOP 1.2.

      Buffer_Out           : Buffers.Buffer_Access;
      Buffer_In            : Buffers.Buffer_Access;
      Role                 : ORB.Endpoint_Role;
      Pending_Rq           : Pend_Req_Seq.Sequence;
      Processing_Rq        : Req_Seq.Sequence;
      Current_Profile      : Profile_Access;
      Object_Found         : Boolean := False;
      --  XXX wrong.
      --  You can have many independent pending Locate_Requests
      --  on the same session!
      Nbr_Tries            : Natural := 0;
      State                : GIOP_State;
      Mess_Type_Received   : Msg_Type;
   end record;

   procedure Finalize (S : in out GIOP_Session);

   procedure Handle_Unmarshall_Arguments
     (Ses : access GIOP_Session;
      Args : in out Any.NVList.Ref);

   --  XXX The components of GIOP session should be documented!

   type GIOP_Protocol is new Protocol with null record;

   Message_Header_Size : constant Stream_Element_Offset := 12;

   Maximum_Message_Size : constant Stream_Element_Offset := 1000;

   Byte_Order_Offset : constant Stream_Element_Offset := 6;

   Max_Data_Received : constant Integer := 1024;

   Max_Nb_Tries : constant Integer := 100;

   subtype Bit_Order_Type is Integer
     range 0 .. Types.Octet'Size;

   function Is_Set
     (Bit_Field : Types.Octet;
      Bit_Order : Bit_Order_Type)
     return Boolean;
   pragma Inline (Is_Set);
   --  True if, and only if, the bit of order
   --  Bit_Order is set in Bit_Field.
   --  (Bit_Order = 0 is the least significant bit).

   procedure Set
     (Bit_Field : in out Types.Octet;
      Bit_Order : Bit_Order_Type;
      Bit_Value : Boolean);
   pragma Inline (Set);
   --  Set the value of bit Bit_Order in Bit_Field
   --  to 1 if Bit_Value = True, to 0 otherwise.

   Endianness_Bit : constant Bit_Order_Type := 0;
   Fragment_Bit   : constant Bit_Order_Type := 1;

   function Get_Request_Id return Types.Unsigned_Long;
   --  Obtain a new, unique request identifier.

end Droopi.Protocols.GIOP;
