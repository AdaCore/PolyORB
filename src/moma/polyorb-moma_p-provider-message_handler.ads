------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                 POLYORB.MOMA_P.PROVIDER.MESSAGE_HANDLER                  --
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

pragma Ada_2005;

--  Actual implementation of the Message_Handler object.
--  It is derived from PolyORB's Minimal_Servant.
--  The call-back purpose of a Message Handler is to receive a Request from
--  the actual message consumer servant when a message is received : this
--  Request can either be Handle (then the message can not be recovered by a
--  call to the Message_Consumer's Receive and has to be treated by the Handle
--  procedure), or Notify (then the message stays in the pool).

with MOMA.Message_Handlers;

with PolyORB.Minimal_Servant;
with PolyORB.Requests;

package PolyORB.MOMA_P.Provider.Message_Handler is

   type Object is new PolyORB.Minimal_Servant.Servant with private;

   type Object_Acc is access Object;

   overriding procedure Invoke
     (Self : access Object;
      Req  : PolyORB.Requests.Request_Access);
   --  Message_Handler servant skeleton.

   procedure Initialize
     (Self                 : access Object;
      MOMA_Message_Handler :        MOMA.Message_Handlers.Message_Handler_Acc);
   --  Initialize with MOMA_Message_Handler.
   --  Should be called after Initiate_Servant.
   --  Should be called only once.

private

   type Object is new PolyORB.Minimal_Servant.Servant with record
      MOMA_Message_Handler : MOMA.Message_Handlers.Message_Handler_Acc := null;
   end record;

end PolyORB.MOMA_P.Provider.Message_Handler;
