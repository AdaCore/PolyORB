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
   --  Request the receiving Servant to execute Req. Oid is
   --  the object Id that was determined to be associated with
   --  the receiving surrogate of the object.
   --  The expected reply is Executed_Request. Null_Message
   --  can also be returned if the request was not processed
   --  immediately.

   type Executed_Request is new Components.Message with record
      Req : Requests.Request_Access;
   end record;
   --  Returned by a servant after Req has been executed.

end PolyORB.Objects.Interface;
