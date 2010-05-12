------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--               P O L Y O R B . P R O T O C O L S . D N S                --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--         Copyright (C) 2002-2010, Free Software Foundation, Inc.          --
--                                                                          --
-- PolyORB is free software; you  can  redistribute  it and/or modify it    --
-- under terms of the  GNU General Public License as published by the  Free --
-- Software Foundation;  either version 2,  or (at your option)  any  later --
-- version. PolyORB is distributed  in the hope that it will be  useful,    --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License  for more details.  You should have received  a copy of the GNU  --
-- General Public License distributed with PolyORB; see file COPYING. If    --
-- not, write to the Free Software Foundation, 51 Franklin Street, Fifth    --
-- Floor, Boston, MA 02111-1301, USA.                                       --
--                                                                          --
-- As a special exception,  if other files  instantiate  generics from this --
-- unit, or you link  this unit with other files  to produce an executable, --
-- this  unit  does not  by itself cause  the resulting  executable  to  be --
-- covered  by the  GNU  General  Public  License.  This exception does not --
-- however invalidate  any other reasons why  the executable file  might be --
-- covered by the  GNU Public License.                                      --
--                                                                          --
--                  PolyORB is maintained by AdaCore                        --
--                     (email: sales@adacore.com)                           --
--                                                                          --
------------------------------------------------------------------------------
with Ada.Streams;
with Ada.Unchecked_Deallocation;
with PolyORB.Buffers;
with PolyORB.ORB;
with PolyORB.Tasking.Mutexes;
with PolyORB.Types;
with PolyORB.Utils.Dynamic_Tables;
with PolyORB.Filters.Iface;
with PolyORB.Utils.Simple_Flags;
--  with PolyORB.Sequences.Unbounded;
--  with PolyORB.Sequences.Unbounded.Helper;
--  pragma Elaborate_All (Polyorb.Sequences.Unbounded.Helper);

package PolyORB.Protocols.DNS is
   use Ada.Streams;
   use PolyORB.Buffers;

   type Flags is new Types.Unsigned_Short;
   package Unsigned_Short_Flags is new PolyORB.Utils.Simple_Flags (Flags);
   use Unsigned_Short_Flags;
   DNS_Error : exception;

   type DNS_Protocol is new Protocol with private;
   type DNS_Session is new Session with private;

   procedure Create
     (Proto   : access DNS_Protocol;
      Session : out Filter_Access);

--  INTERFACE TO UPPER LAYERS
   procedure Invoke_Request
       (Sess   : access DNS_Session;
       R   : Requests.Request_Access;
       Pro : access Binding_Data.Profile_Type'Class);

   procedure Abort_Request
     (S : access DNS_Session;
      R : Requests.Request_Access);

   procedure Send_Reply
     (S : access DNS_Session;
      Request : Requests.Request_Access);

   --  INTERFACE TO LOWER LAYERS
   procedure Handle_Connect_Indication (S : access DNS_Session);

   procedure Handle_Connect_Confirmation (S : access DNS_Session);

   procedure Handle_Data_Indication
     (Sess           : access DNS_Session;
      Data_Amount : Ada.Streams.Stream_Element_Count;
      Error       : in out Errors.Error_Container);

   procedure Handle_Disconnect
     (Sess : access DNS_Session; Error : Errors.Error_Container);

   procedure Handle_Flush (S : access DNS_Session);

   --  DNS protocol proper API

   procedure Initialize_Session
   (S      : access Session'Class);

   procedure Finalize_Session
   (S      : access Session'Class);

   type DNS_Message_Context is abstract tagged private;
   type DNS_Message_Context_Access is access all DNS_Message_Context'Class;

   ----------------
   -- DNS State --
   ----------------

   type DNS_State is
     (Not_Initialized,
      Expect_Header,
      Expect_Body,
      Expect_Name
      );
   type DNS_Data_Expected is
     new PolyORB.Filters.Iface.Data_Expected with record
        State : DNS_State;
     end record;

   -----------------------
   -- DNS message type --
   -----------------------

   type Msg_Type is
     (Request,
      Reply,
      Update);

   type Rcode_Type is
     (No_Error,
      Format_Error,
      Server_Failure,
      Name_Error,
      Not_Implemented,
      Refused,
      YX_Domain,
      YX_RRSet,
      NX_RRSet,
      Not_Auth,
      Not_Zone);

   type Opcode_Type is
     (Query,
      IQuery,
      Status
     );
   type RR_Type is
     (PTR,
      TXT,
      CNAME,
      A);

--     type RR is record
--        rr_name : PolyORB.Types.String;
--        rr_type : PolyORB.Protocols.DNS.RR_Type;
--     end record;
--
--     package SEQUENCE_RR is
--       new PolyORB.Sequences.Unbounded
--         (RR);
--
--     type rrSequence is
--       new SEQUENCE_RR.Sequence;
--     rrSequence_Repository_Id : constant Standard.String :=
--       "IDL:dns/rrSequence:1.0";
--
--     TC_RR_Type : PolyORB.Any.TypeCode.Local_Ref;
--     TC_RR : PolyORB.Any.TypeCode.Local_Ref;
--     TC_SEQUENCE_RR : PolyORB.Any.TypeCode.Local_Ref;
--     function From_Any
--       (C : PolyORB.Any.Any_Container'Class) return RR_Type;
--     function From_Any
--       (Item : PolyORB.Any.Any)
--        return RR_Type;
--
--     function To_Any
--       (Item : RR_Type)
--        return PolyORB.Any.Any;
--
--     function From_Any
--       (Item : PolyORB.Any.Any)
--       return RR;
--
--     function To_Any
--       (Item : RR)
--        return PolyORB.Any.Any;
--
--     function From_Any
--       (Item : PolyORB.Any.Any)
--       return SEQUENCE_RR.Sequence;
--
--     function To_Any
--       (Item : SEQUENCE_RR.Sequence)
--       return PolyORB.Any.Any;
--
--     function From_Any
--       (Item : PolyORB.Any.Any)
--       return rrSequence;
--
--     function To_Any
--       (Item : rrSequence)
--        return PolyORB.Any.Any;
--
--     function SEQUENCE_RR_Element_Wrap
--          (X : access RR)
--          return PolyORB.Any.Content'Class;
--
--     function Wrap
--          (X : access SEQUENCE_RR.Sequence)
--          return PolyORB.Any.Content'Class;
--
--     package SEQUENCE_RR_Helper is
--          new SEQUENCE_RR.Helper
--         (Element_From_Any => PolyORB.Protocols.DNS.From_Any,
--          Element_To_Any => PolyORB.Protocols.DNS.To_Any,
--          Element_Wrap => SEQUENCE_RR_Element_Wrap);
--
--     --  Utilities for the RR_Type type
--     type Ptr_RR_Type is access all RR_Type;
--     type Content_RR_Type is
--          new PolyORB.Any.Aggregate_Content with record
--           V : Ptr_RR_Type;
--           Repr_Cache : aliased PolyORB.Types.Unsigned_Long;
--     end record;
--
--     function Wrap
--          (X : access RR_Type)
--          return PolyORB.Any.Content'Class;
--     function Get_Aggregate_Element
--          (Acc : not null access Content_RR_Type;
--           Tc : PolyORB.Any.TypeCode.Object_Ptr;
--           Index : PolyORB.Types.Unsigned_Long;
--           Mech : not null access PolyORB.Any.Mechanism)
--          return PolyORB.Any.Content'Class;
--
--     procedure Set_Aggregate_Element
--          (Acc : in out Content_RR_Type;
--           Tc : PolyORB.Any.TypeCode.Object_Ptr;
--           Index : PolyORB.Types.Unsigned_Long;
--           From_C : in out PolyORB.Any.Any_Container'Class);
--
--     function Get_Aggregate_Count
--      (Acc : Content_RR_Type)
--          return PolyORB.Types.Unsigned_Long;
--
--     procedure Set_Aggregate_Count
--          (Acc : in out Content_RR_Type;
--           Count : PolyORB.Types.Unsigned_Long);
--     function Clone
--          (Acc : Content_RR_Type;
--           Into : PolyORB.Any.Content_Ptr := null)
--          return PolyORB.Any.Content_Ptr;
--     procedure Finalize_Value (Acc : in out Content_RR_Type);
--
--     --  Utilities for the RR type
--     type Ptr_RR is
--          access all RR;
--          access all RR;
--     type Content_RR is new PolyORB.Any.Aggregate_Content with record
--        V : Ptr_RR;
--     end record;
--        V : Ptr_RR;
--     function Clone
--       (Acc : Content_RR;
--        Into : PolyORB.Any.Content_Ptr := null)
--        return PolyORB.Any.Content_Ptr;
--       (Acc : Content_RR;
--     procedure Finalize_Value
--          (Acc : in out Content_RR);
--          (Acc : in out Content_RR);
--     function Get_Aggregate_Count
--        (Acc : Content_RR)
--         return PolyORB.Types.Unsigned_Long;
--        (Acc : Content_RR)
--     procedure Set_Aggregate_Count
--        (Acc : in out Content_RR;
--         Count : PolyORB.Types.Unsigned_Long);
--        (Acc : in out Content_RR;
--     function Get_Aggregate_Element
--       (Acc : not null access Content_RR;
--        Tc : PolyORB.Any.TypeCode.Object_Ptr;
--        Index : PolyORB.Types.Unsigned_Long;
--        Mech : not null access PolyORB.Any.Mechanism)
--        return PolyORB.Any.Content'Class;
--     function Wrap (X : access RR)
--       return PolyORB.Any.Content'Class;
   -----------------------------------------------------
   procedure Common_Send_Reply
    (Sess           : access DNS_Session;
     Request        : Requests.Request_Access;
     Error          : in out Errors.Error_Container);

   procedure Process_Message
     (S      : access Session'Class);
   procedure Process_Request
     (S : access DNS_Session);

   procedure Initialize;
private
   type Pending_Request is record
      Req            : Requests.Request_Access;
      Request_Id :   Types.Unsigned_Long;
      Target_Profile : Binding_Data.Profile_Access;
   end record;

   type Pending_Request_Access is access all Pending_Request;

   procedure Send_Request
    (S      : access Session'Class;
     R      : Pending_Request_Access;
     Error  : in out Errors.Error_Container);

   procedure Free is new Ada.Unchecked_Deallocation
     (Pending_Request, Pending_Request_Access);

   package Pend_Req_Tables is
      new PolyORB.Utils.Dynamic_Tables
       (Pending_Request_Access, Natural, 1, 10, 10);

   type DNS_Protocol is new Protocol with null record;
   --  For now the DNS message context is exhaustive for all
   --  dns message fields.. to be discussed
   type DNS_Message_Context is abstract tagged record
      Message_Type : Msg_Type;
      Request_Id   : aliased Types.Unsigned_Long;
      Request_Name : Types.String;
      Request_Name_Length : Types.Unsigned_Short;
      Request_Type : RR_Type;
      Request_Class : Types.Unsigned_Short;
      --  DNS Header Flags
      QR_Flag           : Types.Boolean;
      AA_Flag          :  Types.Boolean;
      Opcode_Flag      : Opcode_Type;
      TC_Flag            : Types.Boolean;
      Rec_Flag           : Types.Boolean;
      Rec_Disp_Flag    : Types.Boolean;
      Rcode_Flag         : Rcode_Type;

      Nb_Questions : Types.Unsigned_Short := 0;
      Nb_Responses : Types.Unsigned_Short;
      Nb_Auth_Servers : Types.Unsigned_Short;
      Nb_Add_Infos : Types.Unsigned_Short;
   end record;
   type DNS_Message_Ctx is new DNS_Message_Context with null record;

   type DNS_Session is new Session with record
      Buffer_In : PolyORB.Buffers.Buffer_Access;
      --  DNS Buffer in
      Role   : PolyORB.ORB.Endpoint_Role;
      --  role of session for ORB
      State        : DNS_State := Not_Initialized;
      --  DNS state
      Mutex       : PolyORB.Tasking.Mutexes.Mutex_Access;
      --  DNS message context for the message being received
      Pending_Reqs : Pend_Req_Tables.Instance;
      --  List of pendings request
      Req_Index    : Types.Unsigned_Long := 1;
      --  Counter to have new Request Index
      MCtx : DNS_Message_Context_Access;
   end record;

   type DNS_Session_Access is access all DNS_Session;

   --  annotations used by pending requests
   type Request_Note is new PolyORB.Annotations.Note with record
     Id : Types.Unsigned_Long;
   end record;

   procedure Initialize (S : in out DNS_Session);

   --  Header size of the DNS packet
   DNS_Header_Size : constant Stream_Element_Offset := 12;
   DNS_Max_Size : constant Stream_Element_Offset := 512;

   --  Bit_Count definition for DNS header flags
   QR_Flag_Pos : constant Bit_Count := 1;
   Opcode_Flag_Pos : constant Bit_Count := 2;
   AA_Flag_Pos : constant Bit_Count := 6;
   TC_Flag_Pos : constant Bit_Count := 7;
   Rec_Flag_Pos : constant Bit_Count := 8;
   Rec_Disp_Flag_Pos : constant Bit_Count := 9;
   Res_Flag_Pos : constant Bit_Count := 10;
   Rcode_Flag_Pos : constant Bit_Count := 13;

   procedure Expect_DNS_Header
     (Sess : access DNS_Session);

   procedure Marshall_DNS_Header_Reply
      (Header_Buffer  : access Buffers.Buffer_Type;
      R      :  Requests.Request_Access;
      MCtx : access DNS_Message_Context'Class);
   procedure Marshall_DNS_Header
     (Header_Buffer  : access Buffers.Buffer_Type;
      R      : Pending_Request_Access;
      MCtx : access DNS_Message_Context'Class);
   procedure Unmarshall_DNS_Header
     (MCtx_Acc    : access DNS_Message_Context'Class;
      Buffer  : access Buffers.Buffer_Type);
   procedure Unmarshall_DNS_Request_Message
     (Buffer           : access Buffer_Type;
      MCtx_Acc    : access DNS_Message_Context'Class
     );
   procedure Unmarshall_Argument_List
     (Buffer              :        Buffer_Access;
      Args                : in out Any.NVList.Ref;
      Error               : in out Errors.Error_Container);
   --------------------------------
   -- Pending Request management --
   --------------------------------

   function Get_Request_Id
     (Sess : access DNS_Session) return Types.Unsigned_Long;
   --  Obtain a new, unique request identifier. The caller is responsible
   --  for ensuring that this function is called under mutual exclusion.

   procedure Add_Pending_Request
     (Sess     : access DNS_Session;
      Pend_Req : Pending_Request_Access);
   --  Add Pend_Req to the list of pending requests on S.
   --  The Req and Target_Profile fields must be already initialized; this
   --  procedure sets the Request_Id. The caller is reponsible for ensuring
   --  that this procedure is called under mutual exclusion.

   procedure Get_Pending_Request
     (Sess    : access DNS_Session;
      Id      :        Types.Unsigned_Long;
      Req     :    out Pending_Request;
      Success :    out Boolean;
      Remove  :        Boolean := True);
   --  Retrieve a pending request of Sess by its request id, and
   --  remove it from the list of pending requests if Remove is set to
   --  true. This procedure ensures proper mutual exclusion.
--
   procedure Remove_Pending_Request
     (Sess    : access DNS_Session;
      Id      : Types.Unsigned_Long;
      Success :    out Boolean);
   --  Remove pending request by its request id from the list of pending
   --  requests on Sess. This procedure ensures proper mutual exclusion.

   procedure Emit_Message
     (S      : access Session'Class;
      Buffer : PolyORB.Buffers.Buffer_Access;
      Error  : in out Errors.Error_Container);
   --  Emit message contained in Buffer to lower layer of the protocol stack.
   --  Implementations may override this operation to provide outgoing messages
   --  fragmentation.
end PolyORB.Protocols.DNS;
