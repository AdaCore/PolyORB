------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                    P O L Y O R B . C A L L _ B A C K                     --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--            Copyright (C) 2002 Free Software Foundation, Inc.             --
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

--  $Id$

with Ada.Tags;
with PolyORB.Log;
with PolyORB.Components;
with PolyORB.Requests;
with PolyORB.Objects.Interface;

package body PolyORB.Call_Back is

   use PolyORB.Log;
   use PolyORB.Requests;
   use PolyORB.Objects.Interface;

   package L is new PolyORB.Log.Facility_Log ("polyorb.call_back");
   procedure O (Message : in String; Level : Log_Level := Debug)
     renames L.Output;

   --------------------
   -- Handle_Message --
   --------------------

   function Handle_Message
     (CB_Handler : access Call_Back_Handler;
      S          : PolyORB.Components.Message'Class)
      return PolyORB.Components.Message'Class
   is
      Nothing : Components.Null_Message;
   begin
      pragma Debug (O ("Handling message of type "
                       & Ada.Tags.External_Tag (S'Tag)));

      if S in Executed_Request then
         declare
            Req : Request_Access
              := Executed_Request (S).Req;
         begin
            pragma Debug (O (Requests.Image (Req.all)));

            --  Execute Call Back function
            CB_Handler.CB_Function.all (Req.all, CB_Handler);

            --  Complete Request execution
            Req.Completed := True;

            --  Note : a complete terminaison may be required, it is left to
            --  the call back handler procedure.

         end;
      end if;

      return Nothing;
   end Handle_Message;

   --------------------------
   -- Attach_Request_To_CB --
   --------------------------

   procedure Attach_Request_To_CB
     (Req        : access PolyORB.Requests.Request;
      CB_Handler :        PolyORB.Call_Back.CBH_Access)
   is
   begin
      Req.Requesting_Component :=
        PolyORB.Components.Component_Access (CB_Handler);
   end Attach_Request_To_CB;

   --------------------------
   -- Attach_Handler_To_CB --
   --------------------------

   procedure Attach_Handler_To_CB
     (CB_Handler  : in out PolyORB.Call_Back.Call_Back_Handler;
      CB_Function :        Handler)
   is
   begin
      CB_Handler.CB_Function := CB_Function;
   end Attach_Handler_To_CB;

   ----------------
   -- Notepad_Of --
   ----------------

   function Notepad_Of
     (CB_Handler : access PolyORB.Call_Back.Call_Back_Handler)
     return PolyORB.Annotations.Notepad_Access is
   begin
      return CB_Handler.Notepad'Access;
   end Notepad_Of;

end PolyORB.Call_Back;
