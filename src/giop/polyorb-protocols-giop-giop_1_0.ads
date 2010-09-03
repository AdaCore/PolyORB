------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--      P O L Y O R B . P R O T O C O L S . G I O P . G I O P _ 1 _ 0       --
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

package PolyORB.Protocols.GIOP.GIOP_1_0 is

private

   type GIOP_Implem_1_0 is new GIOP_Implem with null record;
   type GIOP_Implem_1_0_Access is access all GIOP_Implem_1_0'Class;

   --  GIOP 1.0 message context

   type GIOP_Message_Context_1_0 is new GIOP_Message_Context with null record;

   procedure Initialize_Implem
     (Implem : access GIOP_Implem_1_0);

   procedure Initialize_Session
     (Implem : access GIOP_Implem_1_0;
      S      : access Session'Class);

   procedure Finalize_Session
     (Implem : access GIOP_Implem_1_0;
      S      : access Session'Class);

   procedure Unmarshall_GIOP_Header
     (Implem : access GIOP_Implem_1_0;
      MCtx   : access GIOP_Message_Context'Class;
      Buffer : access Buffers.Buffer_Type);

   procedure Marshall_GIOP_Header
     (Implem : access GIOP_Implem_1_0;
      S      : access Session'Class;
      MCtx   : access GIOP_Message_Context'Class;
      Buffer : access Buffers.Buffer_Type);

   procedure Marshall_GIOP_Header_Reply
     (Implem  : access GIOP_Implem_1_0;
      S       : access Session'Class;
      R       : Request_Access;
      MCtx   : access GIOP_Message_Context'Class;
      Buffer  : access Buffers.Buffer_Type);

   procedure Process_Message
     (Implem : access GIOP_Implem_1_0;
      S      : access Session'Class);

   procedure Send_Reply
     (Implem  : access GIOP_Implem_1_0;
      S       : access Session'Class;
      Request :        Requests.Request_Access);

   procedure Locate_Object
     (Implem : access GIOP_Implem_1_0;
      S      : access Session'Class;
      R      :        Pending_Request_Access;
      Error  : in out Errors.Error_Container);

   procedure Send_Request
     (Implem : access GIOP_Implem_1_0;
      S      : access Session'Class;
      R      : Pending_Request_Access;
      Error  : in out Errors.Error_Container);

   procedure Send_Cancel_Request
     (Implem : access GIOP_Implem_1_0;
      S      : access Session'Class;
      R      : Request_Access);

   --  Data alignment

   Data_Alignment_1_0 : constant Buffers.Alignment_Type := Buffers.Align_1;

   --  Principal

   Nobody_Principal : constant Types.String :=
                        Types.To_PolyORB_String ("nobody");

end PolyORB.Protocols.GIOP.GIOP_1_0;
