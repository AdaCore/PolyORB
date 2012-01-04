------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                   P O L Y O R B . C O M P O N E N T S                    --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--         Copyright (C) 2001-2012, Free Software Foundation, Inc.          --
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

--  Abstract components communicating through synchronous messages.

package PolyORB.Components is

   pragma Preelaborate;

   -------------------------------------
   -- Abstract message and components --
   -------------------------------------

   type Message is abstract tagged null record;
   --  The root type for all messages that can be exchanged
   --  between components.

   type Null_Message is new Message with null record;

   type Component is abstract tagged limited private;
   type Component_Access is access all Component'Class;

   function Handle_Message
     (C : not null access Component;
      M : Message'Class) return Message'Class
      is abstract;
   --  Called internally when component C is to receive message M.
   --  Return a reply (possibly Null_Message if no specific contents
   --  are to be returned to the sender) if M has been handled.
   --  Otherwise, exception Unhandled_Message is raised.
   --  Each component type overloads this primitive, and
   --  thus defines its behaviour in terms of replies to
   --  a set of external stimuli (messages).
   --  This subprogram must not be called directly. To send a message
   --  to a component, Emit or Emit_No_Reply must be used.

   procedure Connect
     (Port   : out Component_Access;
      Target :     Component_Access);
   --  Connect Port to Target: when Port is emitted with message
   --  M, Target receives M.

   function Emit
     (Port : Component_Access;
      Msg  : Message'Class)
     return Message'Class;
   --  Emit message Msg on Port. The reply is returned.

   procedure Emit_No_Reply
     (Port : Component_Access;
      Msg  : Message'Class);
   --  Emit message Msg on Port. The expected reply must be
   --  Null_Message, and will be discarded.

   procedure Destroy (Comp : in out Component);
   --  Destroy component Comp

   procedure Destroy (Comp : in out Component_Access);
   --  Destroy the component designated by Comp and deallocate it.

   -------------------------
   -- Component factories --
   -------------------------

   type Component_Factory is access function return Component_Access;

private
   type Component is abstract tagged limited null record;
end PolyORB.Components;
