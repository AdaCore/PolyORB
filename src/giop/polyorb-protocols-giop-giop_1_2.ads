------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--      P O L Y O R B . P R O T O C O L S . G I O P . G I O P _ 1 _ 2       --
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

with PolyORB.Objects;
with PolyORB.References;
with PolyORB.QoS.Code_Sets;
with PolyORB.Utils.Chained_Lists;

package PolyORB.Protocols.GIOP.GIOP_1_2 is

private

   type GIOP_Implem_1_2 is new GIOP_Implem with record
      Max_GIOP_Message_Size : Types.Unsigned_Long;
      Max_Body              : Types.Unsigned_Long;
   end record;

   --  Maximal size for unfragmented messages: by default, no fragmentation

   Default_Max_GIOP_Message_Size_1_2 : constant Integer := Integer'Last;

   --  Fragment reassembly state state

   type Fragment_State is
     (First,      --  Expecting first body fragment
      Req,        --  Expecting request id in fragment header
      Fragment);  --  Expecting fragment body

   --  GIOP 1.2 message context

   package GIOP_Message_Context_Lists is
     new PolyORB.Utils.Chained_Lists
       (T => GIOP_Message_Context_Access,
        Doubly_Chained => True);

   type GIOP_Message_Context_1_2 is new GIOP_Message_Context with record
      Fragmented    : Types.Boolean;

      --  The following components are used while reassembling a fragmented
      --  message

      Frag_State    : Fragment_State := First;
      --  Fragment reassembly state

      Frag_Buf      : Buffers.Buffer_Access;
      --  Reassembly buffer holding body of reassembled message

      Frag_Size     : Types.Unsigned_Long;
      --  Amount of data from (non-first) fragment that corresponds to actual
      --  fragmented payload.

      Frag_Type     : Msg_Type;
      --  Type of the unfragmented message

      Frag_Position : GIOP_Message_Context_Lists.Iterator;
      --  Iterator used to remove this element from the reassembly list when
      --  last fragment is processed.
   end record;

   type GIOP_Session_Context_1_2 is new GIOP_Session_Context with record
      --  For code sets negotiation
      CSN_Complete : Boolean := False;
      CS_Context   : PolyORB.QoS.Code_Sets.QoS_GIOP_Code_Sets_Parameter_Access;
      Reassembly_Contexts : GIOP_Message_Context_Lists.List;
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
     (Implem : access GIOP_Implem_1_2;
      MCtx   : access GIOP_Message_Context'Class;
      Buffer : access Buffers.Buffer_Type);

   procedure Marshall_GIOP_Header
     (Implem  : access GIOP_Implem_1_2;
      S       : access Session'Class;
      MCtx    : access GIOP_Message_Context'Class;
      Buffer  : access Buffers.Buffer_Type);

   procedure Marshall_GIOP_Header_Reply
     (Implem  : access GIOP_Implem_1_2;
      S       : access Session'Class;
      R       : Request_Access;
      MCtx    : access GIOP_Message_Context'Class;
      Buffer  : access Buffers.Buffer_Type);

   procedure Process_Message
     (Implem : access GIOP_Implem_1_2;
      S      : access Session'Class);

   procedure Send_Reply
     (Implem  : access GIOP_Implem_1_2;
      S       : access Session'Class;
      Request :        Requests.Request_Access);

   procedure Emit_Message
     (Implem : access GIOP_Implem_1_2;
      S      : access Session'Class;
      MCtx   : access GIOP_Message_Context'Class;
      Buffer :        Buffers.Buffer_Access;
      Error  : in out Errors.Error_Container);

   procedure Locate_Object
     (Implem : access GIOP_Implem_1_2;
      S      : access Session'Class;
      R      :        Pending_Request_Access;
      Error  : in out Errors.Error_Container);

   procedure Send_Request
     (Implem : access GIOP_Implem_1_2;
      S      : access Session'Class;
      R      : Pending_Request_Access;
      Error  : in out Errors.Error_Container);

   procedure Send_Cancel_Request
     (Implem : access GIOP_Implem_1_2;
      S      : access Session'Class;
      R      : Request_Access);

   Bidirectionnal_GIOP_Not_Implemented : exception;

   --  Reassembly management

   procedure Store_Reassembly_Context
     (SCtx : access GIOP_Session_Context_1_2;
      MCtx : GIOP_Message_Context_Access);
   function Get_Reassembly_Context
     (SCtx : access GIOP_Session_Context_1_2;
      Request_Id : Types.Unsigned_Long) return GIOP_Message_Context_Access;
   procedure Remove_Reassembly_Context
     (SCtx : access GIOP_Session_Context_1_2;
      MCtx : in out GIOP_Message_Context_Access);
   --  XXX documentation required
   --  Note: These subprograms assume exclusive access to SCtx, which is
   --  guaranteed by the fact that they are only ever called within
   --  Handle_Data_Indication.

   --  Synchronisation scope for 1.2

   type Sync_Scope is (NONE, WITH_TRANSPORT, WITH_SERVER, WITH_TARGET);

   --  Different kind of addressing in GIOP 1.2

   type IOR_Addressing_Info is record
      Selected_Profile_Index : Types.Unsigned_Long;
      IOR                    : PolyORB.References.Ref;
   end record;
   type IOR_Addressing_Info_Access is access all IOR_Addressing_Info;

   type Addressing_Disposition is (Key_Addr, Profile_Addr, Reference_Addr);

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

   --  Bits in flags field

   Bit_Fragment   : constant Octet_Flags.Bit_Count := 1;

   --  Data alignment

   Data_Alignment_1_2 : constant Buffers.Alignment_Type := Buffers.Align_8;

   --  Fragment header size

   Frag_Header_Size : constant Types.Unsigned_Long :=
                        Types.Unsigned_Long'Size / Types.Octet'Size;

end PolyORB.Protocols.GIOP.GIOP_1_2;
