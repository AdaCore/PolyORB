------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--               P O L Y O R B . S E R V A N T S . I F A C E                --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--         Copyright (C) 2003-2012, Free Software Foundation, Inc.          --
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

--  The messages supported by Servants (object implementations).

with PolyORB.Binding_Data;
with PolyORB.Components;
with PolyORB.Requests;

package PolyORB.Servants.Iface is

   type Execute_Request is new Components.Message with record
      Req : Requests.Request_Access;
      Pro : PolyORB.Binding_Data.Profile_Access;
   end record;
   --  Request the receiving Servant to execute Req. On the client side, Pro is
   --  the profile of the target object reference that was used to establish a
   --  binding object with the target. The expected reply is Executed_Request,
   --  or Null_Message if the request was not processed immediately.

   type Abort_Request is new Components.Message with record
      Req : Requests.Request_Access;
   end record;
   --  Request the receiving Servant to abort ongoing processing of Req. The
   --  expected reply is Null_Message.

   type Executed_Request is new Components.Message with record
      Req : Requests.Request_Access;
   end record;
   --  Notify the completion of Req's execution. This message can
   --  be a synchronous reply to Execute_Request, or it can be
   --  emitted asynchronously to the requesting component if
   --  Null_Message was returned as the reply for Execute_Request.
   --  Note: for a request that has been transmitted through a binding
   --  object, notifying completion to the requestor may cause the binding
   --  object to be destroyed. Protocol personalities therefore may not
   --  execute any operation on a binding object through which a reply has
   --  been received once they have emitted an Executed_Request message.

   type Acknowledge_Request is new Components.Message with record
      Req : Requests.Request_Access;
   end record;
   --  Acknowledge the reception of Req. This message can be a
   --  synchronous reply to Execute_Request.

end PolyORB.Servants.Iface;
