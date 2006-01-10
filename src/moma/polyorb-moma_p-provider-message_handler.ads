------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                 POLYORB.MOMA_P.PROVIDER.MESSAGE_HANDLER                  --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--         Copyright (C) 2002-2006, Free Software Foundation, Inc.          --
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

   procedure Invoke
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
