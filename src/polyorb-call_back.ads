------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                     P O L Y O R B . C A L L _ B A C K                    --
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

--  Call back component.
--
--  A Call back component act as a request 'bouncer'. It is associated to a
--  call back function that will receive the Executed_Message message in
--  place of the emitter, and will bounce this message to another destination
--  using its handler.

--  $Id$

with PolyORB.Annotations;
with PolyORB.Components;
with PolyORB.Requests;

package PolyORB.Call_Back is

   use PolyORB.Components;

   type Call_Back_Handler is new PolyORB.Components.Component with private;
   type CBH_Access is access all Call_Back_Handler'Class;

   type Handler is access procedure (Req : PolyORB.Requests.Request;
                                     CBH : access Call_Back_Handler);

   function Handle_Message
     (CB_Handler : access Call_Back_Handler;
      S          :        PolyORB.Components.Message'Class)
      return PolyORB.Components.Message'Class;

   procedure Attach_Request_To_CB
     (Req        : access PolyORB.Requests.Request;
      CB_Handler :        PolyORB.Call_Back.CBH_Access);
   --  Attach a specific request to call back component.

   procedure Attach_Handler_To_CB
     (CB_Handler  : in out PolyORB.Call_Back.Call_Back_Handler;
      CB_Function :        Handler);
   --  Attach a handler to call back component.

   function Notepad_Of
     (CB_Handler : access PolyORB.Call_Back.Call_Back_Handler)
     return PolyORB.Annotations.Notepad_Access;

private

   type Call_Back_Handler is new PolyORB.Components.Component with record
      CB_Function : Handler;
      Notepad : aliased Annotations.Notepad;
   end record;

   pragma Inline (Attach_Request_To_CB,
                  Attach_Handler_To_CB,
                  Notepad_Of);


end PolyORB.Call_Back;

