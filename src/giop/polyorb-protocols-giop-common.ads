------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--        P O L Y O R B . P R O T O C O L S . G I O P . C O M M O N         --
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

with PolyORB.Buffers;
with PolyORB.References;
with PolyORB.QoS.Service_Contexts;
with PolyORB.Types;

package PolyORB.Protocols.GIOP.Common is

   package PRQSC renames PolyORB.QoS.Service_Contexts;

   -----------------------
   -- Generic Marshsall --
   -----------------------

   generic
      type Table_Type is (<>);
      type Target_Type is mod <>;
      with procedure Marshall
        (Buffer : access PolyORB.Buffers.Buffer_Type;
         Index  :        Target_Type);
   procedure Generic_Marshall
     (Buffer : access PolyORB.Buffers.Buffer_Type;
      Val    :        Table_Type);

   ------------------------
   -- Generic Unmarshall --
   ------------------------

   generic
      type Table_Type is (<>);
      type Target_Type is mod <>;
      with function Unmarshall
        (Buffer : access PolyORB.Buffers.Buffer_Type) return Target_Type;
   function Generic_Unmarshall
     (Buffer : access PolyORB.Buffers.Buffer_Type) return Table_Type;

   procedure Marshall
     (Buffer : access PolyORB.Buffers.Buffer_Type;
      Val    :        Reply_Status_Type);

   function Unmarshall
     (Buffer : access PolyORB.Buffers.Buffer_Type)
     return Reply_Status_Type;

   procedure Common_Send_Reply
     (Sess           : access GIOP_Session;
      Request        : Requests.Request_Access;
      MCtx           : access GIOP_Message_Context'Class;
      Error          : in out Errors.Error_Container;
      Recovery       : Boolean := False);
   --  Part of processing for sending a result or exception reply that is
   --  shared across all GIOP versions.
   --  For each completed request, this is initially called with Recovery set
   --  False. If an error occurs, a second call is made with Recovery set True.
   --  In the first case, the request is expected to be marked pending on the
   --  Session (if not, it means we have received a cancel request, and we
   --  do not attempt to send a reply). In that case, the request is removed
   --  from the pending list. In the second case, the check is not made, and
   --  a reply is always sent (on the basis that an error occurred during the
   --  first attempt, which means that at that time the request was indeed
   --  pending, otherwise Common_Send_Reply would have returned immediately
   --  with no error).

   type Locate_Reply_Type is
     (Unknown_Object,
      Object_Here,
      Object_Forward,
      Object_Forward_Perm,
      Loc_System_Exception,       --  not implemented, GIOP 1.2 only
      Loc_Need_Addressing_Mode);  --  not implemented, GIOP 1.2 only

   procedure Marshall
     (Buffer : access PolyORB.Buffers.Buffer_Type;
      Val    :        Locate_Reply_Type);

   function Unmarshall
     (Buffer : access PolyORB.Buffers.Buffer_Type)
     return Locate_Reply_Type;

   procedure Common_Locate_Reply
     (Sess               : access GIOP_Session;
      MCtx               : access GIOP_Message_Context'Class;
      Loc_Type           : Locate_Reply_Type;
      Forward_Ref        : References.Ref;
      Error              : in out Errors.Error_Container);

   procedure Common_Process_Locate_Reply
     (Sess              : access GIOP_Session;
      Locate_Request_Id : Types.Unsigned_Long;
      Loc_Type          : Locate_Reply_Type);

   procedure Common_Send_Cancel_Request
     (Sess  : access GIOP_Session;
      R     : Request_Access;
      MCtx  : access GIOP_Message_Context'Class;
      Error : in out Errors.Error_Container);

   procedure Common_Process_Cancel_Request
     (Sess       : access GIOP_Session;
      Request_Id : Types.Unsigned_Long);

   procedure Common_Reply_Received
     (Sess             : access GIOP_Session;
      Request_Id       : Types.Unsigned_Long;
      Reply_Status     : Reply_Status_Type;
      Service_Contexts : PRQSC.QoS_GIOP_Service_Contexts_Parameter_Access);

   --  Helper routines to replace Error Kind

   procedure Replace_Marshal_5_To_Bad_Param_23
     (Error  : in out Errors.Error_Container;
      Status : PolyORB.Errors.Completion_Status);
   --  If Error is Marshhall_E with minor code 5, replace it with Bad_Param_E,
   --  with minor code 23 and set its status to Status, else do nothing.

   procedure Replace_Marshal_5_To_Inv_Objref_2
     (Error  : in out Errors.Error_Container;
      Status : PolyORB.Errors.Completion_Status);
   --  If Error is Marshhall_E with minor code 5, replace it with Inv_Objref_E,
   --  with minor code 2, and set its status to Status, else do nothing.

   ------------------------
   -- Overkill functions --
   ------------------------

   --  Need to be replaced!

   procedure Copy
     (Buf_In  : PolyORB.Buffers.Buffer_Access;
      Buf_Out : PolyORB.Buffers.Buffer_Access;
      Count   : Types.Unsigned_Long);
   --  Copy Count bytes from a buffer to another one

end PolyORB.Protocols.GIOP.Common;
