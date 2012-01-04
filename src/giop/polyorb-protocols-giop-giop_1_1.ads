------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--      P O L Y O R B . P R O T O C O L S . G I O P . G I O P _ 1 _ 1       --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--         Copyright (C) 2002-2012, Free Software Foundation, Inc.          --
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

--  Note: this implementation does not actually support GIOP 1.1
--  fragmentation: incoming fragmented messages won't be accepted, and
--  outgoing messages will never be fragmented.

package PolyORB.Protocols.GIOP.GIOP_1_1 is

private

   type GIOP_Implem_1_1 is new GIOP_Implem with record
      Max_GIOP_Message_Size : Types.Unsigned_Long;
      Max_Body              : Types.Unsigned_Long;
   end record;

   --  GIOP 1.1 context

   type GIOP_Message_Context_1_1 is new GIOP_Message_Context with null record;

   procedure Initialize_Implem
     (Implem : access GIOP_Implem_1_1);

   procedure Initialize_Session
     (Implem : access GIOP_Implem_1_1;
      S      : access Session'Class);

   procedure Finalize_Session
     (Implem : access GIOP_Implem_1_1;
      S      : access Session'Class);

   procedure Unmarshall_GIOP_Header
     (Implem : access GIOP_Implem_1_1;
      MCtx    : access GIOP_Message_Context'Class;
      Buffer  : access Buffers.Buffer_Type);

   procedure Marshall_GIOP_Header
     (Implem  : access GIOP_Implem_1_1;
      S       : access Session'Class;
      MCtx    : access GIOP_Message_Context'Class;
      Buffer  : access Buffers.Buffer_Type);

   procedure Marshall_GIOP_Header_Reply
     (Implem  : access GIOP_Implem_1_1;
      S       : access Session'Class;
      R       : Request_Access;
      MCtx    : access GIOP_Message_Context'Class;
      Buffer  : access Buffers.Buffer_Type);

   procedure Process_Message
     (Implem : access GIOP_Implem_1_1;
      S      : access Session'Class);

   procedure Send_Reply
     (Implem  : access GIOP_Implem_1_1;
      S       : access Session'Class;
      Request :        Requests.Request_Access);

   procedure Locate_Object
     (Implem : access GIOP_Implem_1_1;
      S      : access Session'Class;
      R      :        Pending_Request_Access;
      Error  : in out Errors.Error_Container);

   procedure Send_Request
     (Implem : access GIOP_Implem_1_1;
      S      : access Session'Class;
      R      : Pending_Request_Access;
      Error  : in out Errors.Error_Container);

   procedure Send_Cancel_Request
     (Implem : access GIOP_Implem_1_1;
      S      : access Session'Class;
      R      : Request_Access);

   procedure Marshall_Argument_List
     (Implem              : access GIOP_Implem_1_1;
      Buffer              : Buffers.Buffer_Access;
      Representation      : access
                              Representations.CDR.CDR_Representation'Class;
      Args                : in out Any.NVList.Ref;
      Direction           : Any.Flags;
      First_Arg_Alignment : Buffers.Alignment_Type;
      Error               : in out Errors.Error_Container);

   --  Bits in flags field

   Bit_Fragment   : constant Octet_Flags.Bit_Count := 1;

   --  Data alignment

   Data_Alignment_1_1 : constant Buffers.Alignment_Type := Buffers.Align_1;

   --  Principal

   Nobody_Principal : constant Types.String :=
                        Types.To_PolyORB_String ("nobody");

end PolyORB.Protocols.GIOP.GIOP_1_1;
