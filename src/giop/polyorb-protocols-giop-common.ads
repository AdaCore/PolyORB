------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--        P O L Y O R B . P R O T O C O L S . G I O P . C O M M O N         --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--         Copyright (C) 2002-2005 Free Software Foundation, Inc.           --
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
--                  PolyORB is maintained by AdaCore                        --
--                     (email: sales@adacore.com)                           --
--                                                                          --
------------------------------------------------------------------------------

with PolyORB.Buffers;
with PolyORB.References;
with PolyORB.Request_QoS.Service_Contexts;
with PolyORB.Types;

package PolyORB.Protocols.GIOP.Common is

   package PRQSC renames PolyORB.Request_QoS.Service_Contexts;

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
        (Buffer : access PolyORB.Buffers.Buffer_Type)
        return Target_Type;
   function Generic_Unmarshall
     (Buffer : access PolyORB.Buffers.Buffer_Type)
     return Table_Type;

   --------------------------
   -- Common Process Reply --
   --------------------------

   type Reply_Status_Type is
     (No_Exception,
      User_Exception,
      System_Exception,
      Location_Forward,
      Location_Forward_Perm,    -- 1.2 specific, but not implemented
      Needs_Addressing_Mode);   -- 1.2 specific, but not implemented

   procedure Marshall
     (Buffer : access PolyORB.Buffers.Buffer_Type;
      Val    :        Reply_Status_Type);

   function Unmarshall
     (Buffer : access PolyORB.Buffers.Buffer_Type)
     return Reply_Status_Type;

   procedure Common_Process_Reply
     (Sess           : access GIOP_Session;
      Request        :        Requests.Request_Access;
      Request_Id_Ptr : access Types.Unsigned_Long;
      Reply_Stat_Ptr : access Reply_Status_Type;
      Error          : in out Errors.Error_Container);

   ------------------
   -- Locate Reply --
   ------------------

   type Locate_Reply_Type is
     (Unknown_Object,
      Object_Here,
      Object_Forward,
      Object_Forward_Perm,        --  not implemented, GIOP 1.2 only
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
      Locate_Request_Id  :        Types.Unsigned_Long;
      Loc_Type           :        Locate_Reply_Type;
      Forward_Ref        :        References.Ref);

   procedure Common_Process_Locate_Reply
     (Sess              : access GIOP_Session;
      Locate_Request_Id :        Types.Unsigned_Long;
      Loc_Type          :        Locate_Reply_Type);

   ----------------------------------
   -- Common_Process_Abort_Request --
   ----------------------------------

   procedure Common_Process_Abort_Request
     (Sess : access GIOP_Session;
      R    : in     Request_Access);

   ---------------------------
   -- Common_Reply_Received --
   ---------------------------

   procedure Common_Reply_Received
     (Sess             : access GIOP_Session;
      Request_Id       : in     Types.Unsigned_Long;
      Reply_Status     : in     Reply_Status_Type;
      Service_Contexts : in
        PRQSC.QoS_GIOP_Service_Contexts_Parameter_Access);

   --  Helper routines to replace Error Kind

   procedure Replace_Marshal_5_To_Bad_Param_23
     (Error  : in out Errors.Error_Container;
      Status : in     PolyORB.Errors.Completion_Status);
   --  If Error is Marshhall_E with minor code 5, replace it with
   --  Bad_Param_E, with minor code 23 and set its status to Status,
   --  else do nothing.

   procedure Replace_Marshal_5_To_Inv_Objref_2
     (Error  : in out Errors.Error_Container;
      Status : in     PolyORB.Errors.Completion_Status);
   --  If Error is Marshhall_E with minor code 5, replace it with
   --  Inv_Objref_E, with minor code 2, and set its status to Status,
   --  else do nothing.

   ------------------------
   -- Overkill functions --
   ------------------------

   --  need to be replaced !

   procedure Copy
     (Buf_In  : PolyORB.Buffers.Buffer_Access;
      Buf_Out : PolyORB.Buffers.Buffer_Access;
      Count   : Types.Unsigned_Long);
   --  copy Count bytes from a buffer to another one

end PolyORB.Protocols.GIOP.Common;
