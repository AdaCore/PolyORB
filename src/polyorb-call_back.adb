------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                    P O L Y O R B . C A L L _ B A C K                     --
--                                                                          --
--                                 B o d y                                  --
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

with Ada.Tags;

with PolyORB.Log;
with PolyORB.Servants.Iface;

package body PolyORB.Call_Back is

   use PolyORB.Log;
   use PolyORB.Servants.Iface;
   use PolyORB.Requests;

   package L is new PolyORB.Log.Facility_Log ("polyorb.call_back");
   procedure O (Message : String; Level : Log_Level := Debug)
     renames L.Output;
   function C (Level : Log_Level := Debug) return Boolean
     renames L.Enabled;

   --------------------
   -- Handle_Message --
   --------------------

   overriding function Handle_Message
     (CB_Handler : not null access Call_Back_Handler;
      S          : Components.Message'Class) return Components.Message'Class
   is
      Nothing : Components.Null_Message;
   begin
      pragma Debug (C, O ("Handling message of type "
                       & Ada.Tags.External_Tag (S'Tag)));

      if S in Executed_Request then
         declare
            Req : constant Request_Access
              := Executed_Request (S).Req;
         begin
            pragma Debug (C, O (Requests.Image (Req.all)));

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
