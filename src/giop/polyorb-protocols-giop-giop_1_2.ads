------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--      P O L Y O R B . P R O T O C O L S . G I O P . G I O P _ 1 _ 2       --
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

with PolyORB.Objects;
with PolyORB.References;
with PolyORB.Protocols.GIOP.Common;
pragma Elaborate_All (PolyORB.Protocols.GIOP.Common); --  WAG:3.15

package PolyORB.Protocols.GIOP.GIOP_1_2 is
   use PolyORB.Protocols.GIOP.Common;

   type GIOP_Implem_1_2 is tagged private;

   type GIOP_Implem_1_2_Access is access all GIOP_Implem_1_2'Class;

   type GIOP_Ctx_1_2 is tagged private;

   type GIOP_Ctx_1_2_Access is access all GIOP_Ctx_1_2;

private

   type GIOP_Implem_1_2 is new GIOP_Implem with record
      Max_GIOP_Message_Size : Types.Unsigned_Long;
      Max_Body              : Types.Unsigned_Long;
   end record;

   --  GIOP Message Type

   type Msg_Type is
     (Request,
      Reply,
      Cancel_Request,
      Locate_Request,
      Locate_Reply,
      Close_Connection,
      Message_Error,
      Fragment);

   --  minimal size for fragmented messages

   Default_Max_GIOP_Message_Size_1_2 : constant Integer := 1000;

   --  fragmenting state

   type Fragment_State is
     (None,       --  no current defragmenting
      First,      --  wait for the first body fragment
      Req,        --  wait for the fragment header, the request_id
      Fragment);  --  wait for the body fragment

   --  GIOP 1.2 context

   type GIOP_Ctx_1_2 is new GIOP_Ctx with record
      Message_Type : Msg_Type;
      Fragmented   : Types.Boolean;
      Request_Id   : aliased Types.Unsigned_Long;
      Reply_Status : aliased Reply_Status_Type;
      --  For fragmenting management
      Frag_State   : Fragment_State := None;
      Frag_Type    : Msg_Type;
      Frag_Req_Id  : Types.Unsigned_Long;
      Frag_Size    : Types.Unsigned_Long;
      Frag_Next    : Types.Unsigned_Long;
      Frag_Buf     : PolyORB.Buffers.Buffer_Access;
   end record;

   procedure Initialize_Implem
     (Implem : access GIOP_Implem_1_2);

   procedure Initialize_Session
     (Implem : access GIOP_Implem_1_2;
      S      : access Session'Class);

   procedure Finalize_Session
     (Implem : access GIOP_Implem_1_2;
      S      : access Session'Class);

   procedure Unmarshall_GIOP_Header
     (Implem  : access GIOP_Implem_1_2;
      S       : access Session'Class);

   procedure Marshall_GIOP_Header
     (Implem  : access GIOP_Implem_1_2;
      S       : access Session'Class;
      Buffer  : access PolyORB.Buffers.Buffer_Type);

   procedure Marshall_GIOP_Header_Reply
     (Implem  : access GIOP_Implem_1_2;
      S       : access Session'Class;
      Buffer  : access PolyORB.Buffers.Buffer_Type);

   procedure Process_Message
     (Implem : access GIOP_Implem_1_2;
      S      : access Session'Class);

   procedure Process_Reply
     (Implem  : access GIOP_Implem_1_2;
      S       : access Session'Class;
      Request :        Requests.Request_Access);

   procedure Emit_Message
     (Implem : access GIOP_Implem_1_2;
      S      : access Session'Class;
      Buffer :        PolyORB.Buffers.Buffer_Access);

   procedure Locate_Object
     (Implem : access GIOP_Implem_1_2;
      S      : access Session'Class;
      R      : in     Pending_Request_Access);

   procedure Send_Request
     (Implem : access GIOP_Implem_1_2;
      S      : access Session'Class;
      R      : in     Pending_Request_Access);

   procedure Process_Abort_Request
     (Implem : access GIOP_Implem_1_2;
      S      : access Session'Class;
      R      : in     Request_Access);

   Bidirectionnal_GIOP_Not_Implemented : exception;

   --  Synchornisation scope for 1.2

   type Sync_Scope is (NONE, WITH_TRANSPORT, WITH_SERVER, WITH_TARGET);

   --  Different kind of addressing in GIOP 1.2

   type IOR_Addressing_Info is record
      Selected_Profile_Index : Types.Unsigned_Long;
      IOR                    : PolyORB.References.Ref;
   end record;
   type IOR_Addressing_Info_Access is access all IOR_Addressing_Info;

   type Addressing_Disposition is
     (Key_Addr, Profile_Addr, Reference_Addr);

   type Target_Address (Address_Type : Addressing_Disposition) is record
      case Address_Type is
         when Key_Addr =>
            Object_Key : PolyORB.Objects.Object_Id_Access;
         when Profile_Addr  =>
            Profile : Binding_Data.Profile_Access;
         when Reference_Addr  =>
            Ref : IOR_Addressing_Info_Access;
      end case;
   end record;

   type Target_Address_Access is access all Target_Address;

   --  bits inf flags field

   Bit_Fragment   : constant Octet_Flags.Bit_Count := 1;

   --  Data alignment

   Data_Alignment_1_2 : constant Opaque.Alignment_Type := 8;

   --  Fragment header size

   Frag_Header_Size : constant Stream_Element_Offset :=
     Types.Unsigned_Long'Size / Types.Octet'Size;

end PolyORB.Protocols.GIOP.GIOP_1_2;
