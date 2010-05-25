------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                   P O L Y O R B . C O M P O N E N T S                    --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--         Copyright (C) 2001-2010, Free Software Foundation, Inc.          --
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

--  Abstract components communicating through synchronous messages.

package PolyORB.Components is

   pragma Preelaborate;

   -------------------------------------
   -- Abstract message and components --
   -------------------------------------

   type Message is abstract tagged null record;
   --  The root type for all messages that can be exchanged
   --  between components.

   type Null_Message is new Message with private;

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

   type Null_Message is new Message with null record;
   type Component is abstract tagged limited null record;

end PolyORB.Components;
