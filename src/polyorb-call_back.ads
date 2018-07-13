------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                    P O L Y O R B . C A L L _ B A C K                     --
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

pragma Ada_2012;

--  Call back component.
--
--  A Call back component act as a request 'bouncer'. It is associated to a
--  call back function that will receive the Executed_Message message in
--  place of the emitter, and will bounce this message to another destination
--  using its handler.

with PolyORB.Annotations;
with PolyORB.Components;
with PolyORB.Requests;

package PolyORB.Call_Back is

   type Call_Back_Handler is new PolyORB.Components.Component with private;

   type CBH_Access is access all Call_Back_Handler'Class;

   type Handler is access procedure
     (Req :        PolyORB.Requests.Request;
      CBH : access Call_Back_Handler);

   overriding function Handle_Message
     (CB_Handler : not null access Call_Back_Handler;
      S          : Components.Message'Class) return Components.Message'Class;

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

      Notepad     : aliased Annotations.Notepad;
   end record;

   pragma Inline (Attach_Request_To_CB);
   pragma Inline (Attach_Handler_To_CB);
   pragma Inline (Notepad_Of);

end PolyORB.Call_Back;
