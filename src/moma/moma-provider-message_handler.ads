------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--        M O M A . P R O V I D E R . M E S S A G E _ H A N D L E R         --
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

--  Actual implementation of the Message_Handler object.
--  It is derived from PolyORB's Minimal_Servant.
--  The call-back purpose of a Message Handler is to receive a Request from
--  the actual message consumer servant when a message is received : this
--  Request can either be Handle (then the message can not be recovered by a
--  call to the Message_Consumer's Receive and has to be treated by the Handle
--  procedure), or Notify (then the message stays in the pool).

--  $Id$

with MOMA.Message_Handlers;

with PolyORB.Minimal_Servant;
with PolyORB.Requests;
with PolyORB.Obj_Adapters.Simple;

package MOMA.Provider.Message_Handler is

   type Object is new PolyORB.Minimal_Servant.Servant with private;

   type Object_Acc is access Object;

   procedure Invoke
     (Self : access Object;
      Req  : in     PolyORB.Requests.Request_Access);
   --  Message_Handler servant skeleton.

   function If_Desc
     return PolyORB.Obj_Adapters.Simple.Interface_Description;
   pragma Inline (If_Desc);
   --  Interface description for SOA object adapter.

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

end MOMA.Provider.Message_Handler;
