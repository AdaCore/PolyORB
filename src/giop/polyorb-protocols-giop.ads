-----------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--               P O L Y O R B . P R O T O C O L S . G I O P                --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--         Copyright (C) 2002-2003 Free Software Foundation, Inc.           --
--                                                                          --
-- PolyORB is free software; you  can  redistribute  it and/or modify it    --
-- under terms of the  GNU General Public License as published by the  Free --
-- Software Foundation;  either version 2,  or (at your option)  any  later --
-- version. PolyORB is distributed  in the hope that it will be  useful,    --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License  for more details.  You should have received  a copy of the GNU  --
-- General Public License distributed with PolyORB; see file COPYING. If    --
-- not, write to the Free Software Foundation, 59 Temple Place - Suite 330, --
-- Boston, MA 02111-1307, USA.                                              --
--                                                                          --
-- As a special exception,  if other files  instantiate  generics from this --
-- unit, or you link  this unit with other files  to produce an executable, --
-- this  unit  does not  by itself cause  the resulting  executable  to  be --
-- covered  by the  GNU  General  Public  License.  This exception does not --
-- however invalidate  any other reasons why  the executable file  might be --
-- covered by the  GNU Public License.                                      --
--                                                                          --
--                PolyORB is maintained by ACT Europe.                      --
--                    (email: sales@act-europe.fr)                          --
--                                                                          --
------------------------------------------------------------------------------

with Ada.Streams;   use Ada.Streams;
with Ada.Unchecked_Deallocation;

with PolyORB.Buffers;
with PolyORB.ORB;
with PolyORB.Opaque;
with PolyORB.Sequences.Unbounded;
with PolyORB.Types;
with PolyORB.Utils.Simple_Flags;
with PolyORB.Filters.Interface;

package PolyORB.Protocols.GIOP is

   --  GIOP exceptions
   GIOP_Error : exception;
   GIOP_Bad_Function_Call : exception;
   GIOP_Unknown_Version : exception;


   type GIOP_Session is new Session with private;
   type GIOP_Protocol is new Protocol with private;

   ------------------------
   -- Session primitives --
   ------------------------

   procedure Create
     (Proto   : access GIOP_Protocol;
      Session : out Filter_Access);

   procedure Invoke_Request
     (Sess : access GIOP_Session;
      R    : Requests.Request_Access;
      Pro  : access Binding_Data.Profile_Type'Class);

   procedure Abort_Request
     (Sess : access GIOP_Session;
      R    : Requests.Request_Access);

   procedure Send_Reply
     (Sess : access GIOP_Session;
      R    : Requests.Request_Access);

   procedure Handle_Connect_Indication
     (Sess : access GIOP_Session);

   procedure Handle_Connect_Confirmation
     (Sess : access GIOP_Session);

   procedure Handle_Data_Indication
     (Sess        : access GIOP_Session;
      Data_Amount : Stream_Element_Count);

   procedure Handle_Disconnect
     (Sess : access GIOP_Session);

   procedure Handle_Unmarshall_Arguments
     (Sess : access GIOP_Session;
      Args : in out Any.NVList.Ref);

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
     new PolyORB.Filters.Interface.Data_Expected with record
        State : GIOP_State;
     end record;

private

   type GIOP_Protocol is new Protocol with null record;

   package Octet_Flags is
      new PolyORB.Utils.Simple_Flags (Types.Octet, Types.Shift_Left);

   type Pending_Request is record
      Req            : Requests.Request_Access;
      Locate_Req_Id  : Types.Unsigned_Long;
      Request_Id     : Types.Unsigned_Long;
      Target_Profile : Binding_Data.Profile_Access;
      --  XXX This attribute should be removed, and
      --  Get_Reference_Info on Req.Target should be
      --  used instead when it is necessary to access the target
      --  profile.
   end record;
   type Pending_Request_Access is access Pending_Request;
   procedure Free is new Ada.Unchecked_Deallocation
     (Pending_Request, Pending_Request_Access);
   package Pend_Req_Seq is new Sequences.Unbounded (Pending_Request_Access);

   --------------------
   -- GIOP Send Mode --
   --------------------

   Default_Locate_Then_Request : constant Boolean := True;

   -----------------
   -- GIOP_Implem --
   -----------------

   --  List of supported GIOP_Version
   type GIOP_Version_List is (GIOP_V_1_0, GIOP_V_1_1, GIOP_V_1_2);

   type GIOP_Implem is abstract tagged record
      Version : GIOP_Version_List;
      --  This values must be set at Implem initialization !
      Data_Alignment      : Opaque.Alignment_Type;
      Locate_Then_Request : Boolean;
   end record;
   type GIOP_Implem_Access is access all GIOP_Implem'Class;

   --  Function which are version specific
   --  Must be implemented by each implem
   procedure Initialize_Implem
     (Implem : access GIOP_Implem)
      is abstract;
   --  Initialize global parameters for implem
   --  Called at PolyORB intitailization

   procedure Initialize_Session
     (Implem : access GIOP_Implem;
      S      : access Session'Class)
      is abstract;
   --  Initialize parameters for a session (a ctx for example)
   --  Called at GIOP Session initialization

   procedure Finalize_Session
     (Implem : access GIOP_Implem;
      S      : access Session'Class)
      is abstract;
   --  Finalize for a session (free parameters)

   procedure Unmarshall_GIOP_Header
     (Implem  : access GIOP_Implem;
      S       : access Session'Class)
      is abstract;

   procedure Marshall_GIOP_Header
     (Implem  : access GIOP_Implem;
      S       : access Session'Class;
      Buffer  : access PolyORB.Buffers.Buffer_Type)
      is abstract;

   procedure Process_Message
     (Implem     : access GIOP_Implem;
      S          : access Session'Class)
      is abstract;

   procedure Emit_Message
     (Implem : access GIOP_Implem;
      S      : access Session'Class;
      Buffer :        PolyORB.Buffers.Buffer_Access);
   --  function which emit data to lower layer
   --  can be overidden to fragment messages

   procedure Process_Abort_Request
     (Implem : access GIOP_Implem;
      S      : access Session'Class;
      R      : in     Request_Access)
      is abstract;
   --  cancel a request

   procedure Process_Reply
     (Implem  : access GIOP_Implem;
      S       : access Session'Class;
      Request :        Requests.Request_Access)
      is abstract;

   procedure Send_Request
     (Implem : access GIOP_Implem;
      S      : access Session'Class;
      R      : in     Pending_Request_Access)
      is abstract;
   --  send a request

   procedure Locate_Object
     (Implem : access GIOP_Implem;
      S      : access Session'Class;
      R      : in     Pending_Request_Access)
      is abstract;
   --  send a locate request to loacte an object

   procedure Marshall_Argument_List
     (Implem              : access GIOP_Implem;
      Buffer              :        PolyORB.Buffers.Buffer_Access;
      Args                : in out Any.NVList.Ref;
      Direction           :        Any.Flags;
      First_Arg_Alignment :        Opaque.Alignment_Type);
   --  Internal subprogram: Marshall arguments from Args
   --  into Buf.
   --  Direction may be ARG_IN or ARG_OUT. Only NamedValues
   --  with Arg_Modes equal to either ARG_INOUT or Direction
   --  will be considered. The first argument marshalled will
   --  be aligned on First_Arg_Alignment.

   procedure Unmarshall_Argument_List
     (Implem              : access GIOP_Implem;
      Buffer              :        PolyORB.Buffers.Buffer_Access;
      Args                : in out Any.NVList.Ref;
      Direction           :        Any.Flags;
      First_Arg_Alignment :        PolyORB.Opaque.Alignment_Type);
   --  Internal subprogram: set the values of arguments in
   --  Args by unmarshalling them from Ses.
   --  Direction may be ARG_IN or ARG_OUT. Only NamedValues
   --  with Arg_Modes equal to either ARG_INOUT or Direction
   --  will be considered. The first argument is assumed to
   --  be aligned on First_Arg_Alignment.
--  functions used to factorize code

   procedure Marshall_GIOP_Header_Reply
     (Implem  : access GIOP_Implem;
      S       : access Session'Class;
      Buffer  : access PolyORB.Buffers.Buffer_Type)
      is abstract;

   ------------------------------------------------

   ------------------
   -- GIOP Version --
   ------------------

   --  GIOP_Send_Mode : constant GIOP_Send_Mode_Type := Direct;
   type GIOP_Version is record
      Major : Types.Octet;
      Minor : Types.Octet;
   end record;

   --  Default GIOP_Version
   GIOP_Default_Version : constant GIOP_Version
     := (Major => 1,
         Minor => 2);
   Current_GIOP_Default_Version : GIOP_Version;

   --  Bind between GIOP_Version_List and GIOP_Version
   GIOP_Version_Bind :
     constant array (GIOP_Version_List) of GIOP_Version
     := (GIOP_V_1_0 => GIOP_Version'(Major => 1, Minor => 0),
         GIOP_V_1_1 => GIOP_Version'(Major => 1, Minor => 1),
         GIOP_V_1_2 => GIOP_Version'(Major => 1, Minor => 2)
         );

   --  Bind between GIOP_Version_List and GIOP_Implem
   GIOP_Version_Access :
     array (GIOP_Version_List) of GIOP_Implem_Access
     := (others => null);

   --  Giop Context
   --  will be extended by each implem
   type GIOP_Ctx is tagged record
      Message_Endianness : PolyORB.Buffers.Endianness_Type
        := PolyORB.Buffers.Host_Order;
      Message_Size       : Types.Unsigned_Long;
   end record;
   type GIOP_Ctx_Access is access all GIOP_Ctx'Class;

   --  Register a GIOP Implem into GIOP_Version_Access
   procedure Register_GIOP_Version
     (Version : GIOP_Version_List;
      Implem  : GIOP_Implem_Access);

   --  Get a GIOP_Implem from GIOP Version
   procedure Get_GIOP_Implem
     (Sess    : access GIOP_Session;
      Version :        GIOP_Version);

   ---------------------------------------------------

   ------------------
   -- GIOP_Session --
   ------------------

   type GIOP_Session is new Session with record
      Implem       : GIOP_Implem_Access;
      State        : GIOP_State := Not_Initialized;
      Ctx          : GIOP_Ctx_Access;
      Buffer_In    : Buffers.Buffer_Access;
      Role         : ORB.Endpoint_Role;
      Pending_Reqs : Pend_Req_Seq.Sequence;
      Req_Index    : Types.Unsigned_Long := 1;
   end record;
   type GIOP_Session_Access is access all GIOP_Session;

   procedure Initialize (S : in out GIOP_Session);
   procedure Finalize (S : in out GIOP_Session);

   --  Magic identifier
   --  Begin of all GIOP Messages
   Magic : constant Stream_Element_Array (1 .. 4)
     := (Character'Pos ('G'),
         Character'Pos ('I'),
         Character'Pos ('O'),
         Character'Pos ('P'));

   --  Header size of GIOP_packet (non version specific header)
   GIOP_Header_Size : constant Stream_Element_Offset := 12;

   --  Place of endianess in a giop packet
   Flags_Index    : constant Stream_Element_Offset := 7;
   Bit_Endianness : constant Octet_Flags.Bit_Count := 0;

   ---------------------------------------------------

   ---------------------------
   -- Global GIOP Functions --
   ---------------------------

   --  GIOP_Header, non specific version
   procedure Unmarshall_Global_GIOP_Header
     (Buffer  : access Buffers.Buffer_Type;
      Version :    out GIOP_Version);

   procedure Marshall_Global_GIOP_Header
     (Sess   : access GIOP_Session;
      Buffer : access PolyORB.Buffers.Buffer_Type);

   procedure Marshall_Global_GIOP_Header
     (Version :        GIOP_Version;
      Buffer  : access PolyORB.Buffers.Buffer_Type);

   --  Prepare S to receive next GIOP message.
   --  This must be called once when a session is established
   --  (in Handle_Connect_Indication for a server session,
   --  in Handle_Connect_Confirmation for a client session),
   --  and then exactly once after a message has been received.
   --  This must not be called after sending a message (because
   --  message sends and receives can be interleaved in an
   --  arbitrary way, and Expect_Message must not be called
   --  twice in a row).
   procedure Expect_GIOP_Header
     (Sess : access GIOP_Session);

   --  A note can be attached to a PolyORB request to augment
   --  it with personality-specific information. The GIOP stack
   --  uses such a note to associate the Request with its
   --  Request_Id.
   type Request_Note is new PolyORB.Annotations.Note with record
     Id : Types.Unsigned_Long;
   end record;

   --  cancel all current requests
   procedure Cancel_Pending_Request
     (Sess : access GIOP_Session);

   function Select_Profile
     (Buffer  : access PolyORB.Buffers.Buffer_Type)
     return PolyORB.Binding_Data.Profile_Access;

   --------------------------------
   -- Pending Request management --
   --------------------------------

   function Get_Request_Id
     (Sess : access GIOP_Session)
     return Types.Unsigned_Long;
   --  Obtain a new, unique request identifier.

   procedure Add_Pending_Request
     (Sess     : access GIOP_Session;
      Pend_Req : in     Pending_Request_Access);
   --  Add Pend_Req to the list of pending requests on S.
   --  The Req and Target_Profile fields must be already
   --  initialized; this procedure sets the Request_Id.

   procedure Get_Pending_Request
     (Sess    : access GIOP_Session;
      Id      :        Types.Unsigned_Long;
      Req     :    out Pending_Request_Access;
      Success :    out Boolean);
   --  Retrieve a pending request of Ses by its request id.

   procedure Get_Pending_Request_By_Locate
     (Sess    : access GIOP_Session;
      Id      :        Types.Unsigned_Long;
      Req     :    out Pending_Request_Access;
      Success :    out Boolean);
   --  Retrieve a pending request of Ses by its locate request id.

   procedure Free_Pending_Request
     (Sess : access GIOP_Session;
      Id   :        Types.Unsigned_Long);
   --  Free the pending request

   ---------------------------------
   -- Marshall Unmarshall helpers --
   ---------------------------------

   --  Context_List
   procedure Marshall_Service_Context_List
     (Buffer : access Buffers.Buffer_Type);

   procedure Unmarshall_Service_Context_List
     (Buffer : access Buffers.Buffer_Type);
   --  XXX Dummy unmarshalling for Service Context List.

   procedure Unmarshall_System_Exception_To_Any
     (Buffer : PolyORB.Buffers.Buffer_Access;
      Info   : out Any.Any);
   --  umarshall exception info

   function Get_Conf_Chain
     (Implem : access GIOP_Implem'Class)
     return String;

end PolyORB.Protocols.GIOP;
