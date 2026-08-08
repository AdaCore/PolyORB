------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--               P O L Y O R B . P R O T O C O L S . G I O P                --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--         Copyright (C) 2002-2013, Free Software Foundation, Inc.          --
--                                                                          --
-- This is free software;  you can redistribute it  and/or modify it  under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  This software is distributed in the hope  that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License for  more details.                                               --
--                                                                          --
-- As a special exception under Section 7 of GPL version 3, you are granted --
-- additional permissions described in the GCC Runtime Library Exception,   --
-- version 3.1, as published by the Free Software Foundation.               --
--                                                                          --
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
--                                                                          --
--                  PolyORB is maintained by AdaCore                        --
--                     (email: sales@adacore.com)                           --
--                                                                          --
------------------------------------------------------------------------------

pragma Ada_2012;

with Ada.Streams;
with PolyORB.Utils.Unchecked_Deallocation;

with PolyORB.Binding_Data;
with PolyORB.Buffers;
with PolyORB.Errors;
with PolyORB.Filters.Iface;
with PolyORB.ORB;
with PolyORB.QoS;
with PolyORB.Representations.CDR;
with PolyORB.Requests;
with PolyORB.Tasking.Mutexes;
with PolyORB.Transport;
with PolyORB.Types;
with PolyORB.Utils.Dynamic_Tables;
with PolyORB.Utils.Simple_Flags;

package PolyORB.Protocols.GIOP is

   use Ada.Streams;

   GIOP_Error : exception;

   type GIOP_Session is new Session with private;
   type GIOP_Protocol is abstract new Protocol with private;

   ------------------------
   -- Version management --
   ------------------------

   --  ??? How about other major versions
   --  ??? How about other minor versions

   type GIOP_Version is (GIOP_V1_0, GIOP_V1_1, GIOP_V1_2);
   --  Must be kept in ascending order

   To_GIOP_Version : constant array (0 .. 2) of GIOP_Version
     := (0 => GIOP_V1_0, 1 => GIOP_V1_1, 2 => GIOP_V1_2);

   To_Minor_GIOP : constant array (GIOP_Version) of Types.Octet
     := (GIOP_V1_0 => 0, GIOP_V1_1 => 1, GIOP_V1_2 => 2);

   ------------------------
   -- Session primitives --
   ------------------------

   overriding
   procedure Create
     (Proto   : access GIOP_Protocol;
      Session :    out Filter_Access);

   overriding
   procedure Invoke_Request
     (Sess : access GIOP_Session;
      R    :        Requests.Request_Access;
      Pro  : access Binding_Data.Profile_Type'Class);

   overriding
   procedure Abort_Request
     (Sess : access GIOP_Session;
      R    :        Requests.Request_Access);

   overriding
   procedure Send_Reply
     (Sess : access GIOP_Session;
      R    :        Requests.Request_Access);

   procedure Locate_Object
     (Sess    : access GIOP_Session;
      Profile : Binding_Data.Profile_Access;
      Error   : in out Errors.Error_Container);

   overriding
   procedure Handle_Connect_Indication
     (Sess : access GIOP_Session);

   overriding
   procedure Handle_Connect_Confirmation
     (Sess : access GIOP_Session);

   overriding
   procedure Handle_Data_Indication
     (Sess        : access GIOP_Session;
      Data_Amount : Stream_Element_Count;
      Error       : in out Errors.Error_Container);

   overriding
   procedure Handle_Disconnect
     (Sess : access GIOP_Session; Error : Errors.Error_Container);

   overriding
   procedure Handle_Unmarshall_Arguments
     (Sess  : access GIOP_Session;
      Args  : in out Any.NVList.Ref;
      Error : in out Errors.Error_Container);

   overriding
   procedure Handle_Flush (Sess : access GIOP_Session);

   ----------------
   -- GIOP State --
   ----------------

   type GIOP_State is
     (Not_Initialized,        --  Session initialized
      Expect_Header,          --  Waiting for a new message header
      Expect_Body,            --  Waiting for body message
      Waiting_Unmarshalling   --  Waiting argument unsmarshalling
      );

   type GIOP_Data_Expected is
     new PolyORB.Filters.Iface.Data_Expected with record
        State : GIOP_State;
     end record;

   -----------------------
   -- GIOP message type --
   -----------------------

   type Msg_Type is
     (Request,
      Reply,
      Cancel_Request,
      Locate_Request,
      Locate_Reply,
      Close_Connection,
      Message_Error,
      Fragment); -- Not available for GIOP 1.0

   --------------------------
   -- GIOP message context --
   --------------------------

   type GIOP_Message_Context is abstract tagged private;
   type GIOP_Message_Context_Access is access all GIOP_Message_Context'Class;

   type Reply_Status_Type is
     (No_Exception,
      User_Exception,
      System_Exception,
      Location_Forward,
      Location_Forward_Perm,
      Needs_Addressing_Mode);   -- 1.2 specific, but not implemented

   --  Security Service Hooks

   type Fetch_Secure_Transport_QoS_Hook is
     access function (End_Point : PolyORB.Transport.Transport_Endpoint_Access)
       return PolyORB.QoS.QoS_Parameter_Access;

   Fetch_Secure_Transport_QoS : Fetch_Secure_Transport_QoS_Hook := null;

   function Get_Representation
     (Sess : access GIOP_Session)
     return PolyORB.Representations.CDR.CDR_Representation_Access;
   --  Return the representation object used by the session.
   --  Note: the user is not allowed to destroy this object

   function Get_Buffer
     (Sess : access GIOP_Session)
     return PolyORB.Buffers.Buffer_Access;
   --  Return the buffer object used by the session.
   --  Note: the user is not allowed to destroy this object

private

   use PolyORB.Types;

   type GIOP_Protocol is abstract new Protocol with null record;

   package Octet_Flags is new PolyORB.Utils.Simple_Flags (Types.Octet);

   type Pending_Request is record
      Req            : Requests.Request_Access;
      Locate_Req_Id  : Types.Unsigned_Long := 0;
      Request_Id     : Types.Unsigned_Long := 0;
      Target_Profile : Binding_Data.Profile_Access;
      --  XXX This attribute should be removed, and Get_Reference_Info on
      --  Req.Target should be used instead when it is necessary to access the
      --  target profile.
   end record;
   type Pending_Request_Access is access all Pending_Request;

   procedure Free is new PolyORB.Utils.Unchecked_Deallocation.Free
     (Object => Pending_Request,
      Name => Pending_Request_Access);

   package Pend_Req_Tables is new
     PolyORB.Utils.Dynamic_Tables (Pending_Request_Access, Natural, 1, 10, 10);

   --------------------
   -- GIOP send mode --
   --------------------

   Default_Locate_Then_Request : constant Boolean := True;

   --  Default GIOP_Version

   GIOP_Default_Version : constant GIOP_Version := GIOP_V1_2;

   procedure Get_GIOP_Implem
     (Sess            : access GIOP_Session;
      Version         : GIOP_Version;
      Allow_Downgrade : Boolean := False);
   --  Retrieve a GIOP_Implem for the specified GIOP Version, and associate
   --  it with Sess. If Allow_Downgrade is True, and the given Version is
   --  unavailable, try a lower version.

   --------------------------
   -- GIOP message context --
   --------------------------

   --  Version-specific information associated with a GIOP message

   type GIOP_Message_Context is abstract tagged record
      Message_Endianness : PolyORB.Buffers.Endianness_Type :=
                             PolyORB.Buffers.Host_Order;
      Message_Type : Msg_Type;
      Message_Size : Types.Unsigned_Long;
      Request_Id   : aliased Types.Unsigned_Long;
      Reply_Status : Reply_Status_Type;
   end record;

   procedure Free is new PolyORB.Utils.Unchecked_Deallocation.Free
     (Object => GIOP_Message_Context'Class,
      Name => GIOP_Message_Context_Access);

   ---------------------------
   --  GIOP session context --
   ---------------------------

   --  Version-specific information associated with a GIOP sesssion

   type GIOP_Session_Context is abstract tagged null record;
   type GIOP_Session_Context_Access is access all GIOP_Session_Context'Class;

   procedure Free is new PolyORB.Utils.Unchecked_Deallocation.Free
     (Object => GIOP_Session_Context'Class,
      Name => GIOP_Session_Context_Access);

   -----------------
   -- GIOP_Implem --
   -----------------

   --  A GIOP implementation encapsulates the version-specific behaviour
   --  of a GIOP stack.

   type GIOP_Implem is abstract tagged record
      Version               : GIOP_Version;
      --  This values must be set at Implem initialization

      Data_Alignment        : Buffers.Alignment_Type;
      Locate_Then_Request   : Boolean;
      --  Configuration values

      Section               : Types.String;
      Prefix                : Types.String;
      --  XXX ??? what are these?

      Permitted_Sync_Scopes : PolyORB.Requests.Flags;
      --  Allowed Req Flags
   end record;
   type GIOP_Implem_Access is access all GIOP_Implem'Class;

   procedure Initialize_Implem
     (Implem : access GIOP_Implem)
      is abstract;
   --  Initialize global parameters for Implem
   --  Called at PolyORB initialization

   procedure Initialize_Session
     (Implem : access GIOP_Implem;
      S      : access Session'Class)
      is abstract;
   --  Initialize parameters for a session
   --  Called at GIOP Session initialization

   procedure Finalize_Session
     (Implem : access GIOP_Implem;
      S      : access Session'Class)
      is abstract;
   --  Finalize for a session (free parameters)

   procedure Unmarshall_GIOP_Header
     (Implem : access GIOP_Implem;
      MCtx   : access GIOP_Message_Context'Class;
      Buffer : access Buffers.Buffer_Type)
      is abstract;

   procedure Marshall_GIOP_Header
     (Implem  : access GIOP_Implem;
      S       : access Session'Class;
      MCtx    : access GIOP_Message_Context'Class;
      Buffer  : access Buffers.Buffer_Type)
      is abstract;

   procedure Process_Message
     (Implem     : access GIOP_Implem;
      S          : access Session'Class)
      is abstract;

   procedure Emit_Message
     (Implem : access GIOP_Implem;
      S      : access Session'Class;
      MCtx   : access GIOP_Message_Context'Class;
      Buffer : PolyORB.Buffers.Buffer_Access;
      Error  : in out Errors.Error_Container);
   --  Emit message contained in Buffer to lower layer of the protocol stack.
   --  Implementations may override this operation to provide outgoing messages
   --  fragmentation.

   procedure Send_Cancel_Request
     (Implem : access GIOP_Implem;
      S      : access Session'Class;
      R      : Request_Access)
      is abstract;
   --  Cancel a request

   procedure Send_Reply
     (Implem  : access GIOP_Implem;
      S       : access Session'Class;
      Request :        Requests.Request_Access)
      is abstract;
   --  Send a reply

   procedure Send_Request
     (Implem : access GIOP_Implem;
      S      : access Session'Class;
      R      :        Pending_Request_Access;
      Error  : in out Errors.Error_Container)
      is abstract;
   --  Send a request

   procedure Locate_Object
     (Implem : access GIOP_Implem;
      S      : access Session'Class;
      R      :        Pending_Request_Access;
      Error  : in out Errors.Error_Container)
      is abstract;
   --  Send a locate request to locate an object

   procedure Marshall_Argument_List
     (Implem              : access GIOP_Implem;
      Buffer              : Buffers.Buffer_Access;
      Representation      : access
                              Representations.CDR.CDR_Representation'Class;
      Args                : in out Any.NVList.Ref;
      Direction           : Any.Flags;
      First_Arg_Alignment : Buffers.Alignment_Type;
      Error               : in out Errors.Error_Container);
   --  Internal subprogram: Marshall arguments from Args into Buf.
   --  Direction may be ARG_IN or ARG_OUT. Only NamedValues with Arg_Modes
   --  equal to either ARG_INOUT or Direction will be considered. The first
   --  argument marshalled will be aligned on First_Arg_Alignment.

   procedure Unmarshall_Argument_List
     (Implem              : access GIOP_Implem;
      Buffer              : Buffers.Buffer_Access;
      Representation      : access
                              Representations.CDR.CDR_Representation'Class;
      Args                : in out Any.NVList.Ref;
      Direction           : Any.Flags;
      First_Arg_Alignment : Buffers.Alignment_Type;
      Error               : in out Errors.Error_Container);
   --  Internal subprogram: set the values of arguments in
   --  Args by unmarshalling them from Ses.
   --  Direction may be ARG_IN or ARG_OUT. Only NamedValues with Arg_Modes
   --  equal to either ARG_INOUT or Direction will be considered. The first
   --  argument is assumed to be aligned on First_Arg_Alignment.

   procedure Marshall_GIOP_Header_Reply
     (Implem  : access GIOP_Implem;
      S       : access Session'Class;
      R       : Request_Access;
      MCtx    : access GIOP_Message_Context'Class;
      Buffer  : access PolyORB.Buffers.Buffer_Type) is abstract;

   --  GIOP Implem management

   type GIOP_Factory is access function return GIOP_Implem_Access;

   type GIOP_Implem_Array is array (GIOP_Version) of GIOP_Implem_Access;

   procedure Global_Register_GIOP_Version
     (Version : GIOP_Version;
      Implem  : GIOP_Factory);

   ------------------------
   -- GIOP configuration --
   ------------------------

   type GIOP_Conf is record
      GIOP_Default_Version  : GIOP_Version;
      --  Default GIOP Version

      GIOP_Implems      : GIOP_Implem_Array;
      --  List of activated GIOP Implem

      Permitted_Sync_Scopes : PolyORB.Requests.Flags;
      --  Allowed Req Flags
   end record;

   type GIOP_Conf_Access is access all GIOP_Conf;

   procedure Initialize
     (Conf                  : access GIOP_Conf;
      Version               : GIOP_Version;
      Permitted_Sync_Scopes : PolyORB.Requests.Flags;
      Locate_Then_Request   : Boolean;
      Section               : String;
      Prefix                : String);
   --  Initialize a GIOP Configuration, reading PolyORB configuration

   ------------------
   -- GIOP_Session --
   ------------------

   type GIOP_Session is new Session with record
      Implem       : GIOP_Implem_Access;
      --  Access to current implem

      Repr         : Representations.CDR.CDR_Representation_Access;
      --  Marshalling/unmarshalling representation object

      State        : GIOP_State := Not_Initialized;
      --  GIOP state

      SCtx         : GIOP_Session_Context_Access;
      --  GIOP session context, implem dependant

      MCtx         : GIOP_Message_Context_Access;
      --  GIOP message context for the message being received

      Buffer_In    : Buffers.Buffer_Access;
      --  GIOP Buffer in

      Role         : ORB.Endpoint_Role;
      --  Role of session for ORB

      Conf         : GIOP_Conf_Access;
      --  Configuration parameters

      --------------------------------------
      -- Global state of the GIOP session --
      --------------------------------------

      --  These components must be accessed under mutual exclusion at
      --  the session level.

      Mutex : Tasking.Mutexes.Mutex_Access;
      --  Critical section for concurrent access to Pending_Reqs and
      --  Req_Index.

      Pending_Reqs : Pend_Req_Tables.Instance;
      --  List of pendings requests. Note: when Bidirectional_GIOP is
      --  implemented, this component will need to be split into a client-side
      --  and a server-side list.
      --  Investigate usage of an Ordered_Map container???

      Req_Index    : Types.Unsigned_Long := 1;
      --  Request Id for next request
   end record;
   type GIOP_Session_Access is access all GIOP_Session;

   procedure Initialize (S : in out GIOP_Session);
   overriding procedure Destroy (S : in out GIOP_Session);

   --  Magic identifier: 4 bytes at the begining of every GIOP message

   Magic : constant Stream_Element_Array (1 .. 4)
     := (Character'Pos ('G'),
         Character'Pos ('I'),
         Character'Pos ('O'),
         Character'Pos ('P'));

   GIOP_Header_Size : constant Stream_Element_Offset := 12;
   --  Header size of GIOP_packet (non version specific header)

   GIOP_Fixed_Part_Size : constant Stream_Element_Offset := 6;
   --  Size of the fixed GIOP_packet header (version specific header)

   Request_Id_Size : constant := 4;

   Flags_Index       : constant Stream_Element_Offset := 7;
   Bit_Little_Endian : constant Octet_Flags.Bit_Count := 0;
   --  Location of flags in GIOP packet

   ---------------------------
   -- Global GIOP Functions --
   ---------------------------

   procedure Unmarshall_Global_GIOP_Header
     (Sess    : access GIOP_Session;
      Buffer  : access Buffers.Buffer_Type;
      Version : out GIOP_Version);
   --  XXX description required

   procedure Marshall_Global_GIOP_Header
     (Sess   : access GIOP_Session;
      MCtx   : access GIOP_Message_Context'Class;
      Buffer : access PolyORB.Buffers.Buffer_Type);
   --  XXX description required

   procedure Expect_GIOP_Header (Sess : access GIOP_Session);
   --  Prepare S to receive next GIOP message.
   --  This must be called once when a session is established (in
   --  Handle_Connect_Indication for a server session, in
   --  Handle_Connect_Confirmation for a client session), and then exactly
   --  once after a message has been received.  This must not be called after
   --  sending a message (because message sends and receives can be
   --  interleaved in an arbitrary way, and Expect_Message must not be called
   --  twice in a row). The caller must guarantee that the binding object
   --  terminated by Sess will persist during the execution of
   --  Expect_GIOP_Header.

   type Request_Note is new PolyORB.Annotations.Note with record
     Id : Types.Unsigned_Long;
   end record;
   --  A note can be attached to a PolyORB request to augment
   --  it with personality-specific information. The GIOP stack
   --  uses such a note to associate the Request with its
   --  Request_Id.

   procedure Queue_Request
     (Sess   : access GIOP_Session;
      Req    : Request_Access;
      Req_Id : Types.Unsigned_Long);
   --  Queue Req for further processing by ORB (usually for service by a local
   --  servant). Req is added to the (server-side) pending requests list
   --  associated with Sess.

   --------------------------------
   -- Pending Request management --
   --------------------------------

   function Get_Request_Id
     (Sess : access GIOP_Session) return Types.Unsigned_Long;
   --  Obtain a new, unique request identifier. The caller is responsible
   --  for ensuring that this function is called under mutual exclusion.

   procedure Add_Pending_Request
     (Sess     : access GIOP_Session;
      Pend_Req : Pending_Request_Access);
   --  Add Pend_Req to the list of pending requests on S.
   --  The Req and Target_Profile fields must be already initialized; this
   --  procedure sets the Request_Id. The caller is reponsible for ensuring
   --  that this procedure is called under mutual exclusion.

   procedure Get_Pending_Request
     (Sess    : access GIOP_Session;
      Id      : Types.Unsigned_Long;
      Req     : out Pending_Request;
      Success : out Boolean);
   --  Retrieve a pending request of Sess by its request id, and remove it from
   --  the list of pending requests. The caller is reponsible for ensuring
   --  that this procedure is called under mutual exclusion.

   procedure Get_Pending_Request_By_Locate
     (Sess    : access GIOP_Session;
      L_Id    : Types.Unsigned_Long;
      Req     : out Pending_Request_Access;
      Success : out Boolean;
      Remove  : Boolean);
   --  Retrieve a pending request of Sess by its locate request id, and remove
   --  it from the list of pending requests if Remove is set True. In this
   --  case, Req is set to null on return. This procedure ensures proper mutual
   --  exclusion.

   procedure Remove_Pending_Request
     (Sess    : access GIOP_Session;
      Id      : Types.Unsigned_Long;
      Success : out Boolean);
   --  Remove pending request by its request id from the list of pending
   --  requests on Sess. This procedure ensures proper mutual exclusion.

   procedure Remove_Pending_Request_By_Locate
     (Sess    : access GIOP_Session;
      Id      : Types.Unsigned_Long;
      Success :    out Boolean);
   --  Remove pending request by locate request id from the list of pending
   --  requests on Sess. This procedure ensures proper mutual exclusion.

   ---------------------------------
   -- Marshall Unmarshall helpers --
   ---------------------------------

   procedure Unmarshall_System_Exception_To_Any
     (Buffer : Buffers.Buffer_Access;
      Repr   : access Representations.CDR.CDR_Representation'Class;
      Info   : out Any.Any);

   function Get_Conf_Chain (Implem : access GIOP_Implem'Class) return String;

end PolyORB.Protocols.GIOP;
