------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--            P O L Y O R B . O B J E C T S . I N T E R F A C E             --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                Copyright (C) 2001 Free Software Fundation                --
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
--              PolyORB is maintained by ENST Paris University.             --
--                                                                          --
------------------------------------------------------------------------------

--  The messages supported by Servants (object implementations).

--  $Id$

with PolyORB.Binding_Data;
with PolyORB.Components;
with PolyORB.Requests;

package PolyORB.Objects.Interface is

   type Execute_Request is new Components.Message with record
      Req : Requests.Request_Access;
      Pro : PolyORB.Binding_Data.Profile_Access;
   end record;
   --  Request the receiving Servant to execute Req. On the client
   --  side, Pro is the profile of the target object reference that
   --  was used to establish a binding object with the target.
   --  The expected reply is Executed_Request, or Null_Message
   --  if the request was not processed immediately.

   type Executed_Request is new Components.Message with record
      Req : Requests.Request_Access;
   end record;
   --  Notify the completion of Req's execution. This message can
   --  be a synchronous reply to Execute_Request, or it can be
   --  emitted asynchronously to the requesting component if
   --  Null_Message was returned as the reply for Execute_Request.

end PolyORB.Objects.Interface;
