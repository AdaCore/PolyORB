------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                   P O L Y O R B . C O M P O N E N T S                    --
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

--  Abstract components communicating through synchronous messages.

--  $Id$

with Ada.Finalization;

with PolyORB.Sequences.Unbounded;

package PolyORB.Components is

   pragma Elaborate_Body;

   -------------------------------------
   -- Abstract message and components --
   -------------------------------------

   type Message is abstract tagged null record;
   --  The root type for all messages that can be exchanged
   --  between components.

   type Null_Message is new Message with private;

   type Component is
     abstract new Ada.Finalization.Limited_Controlled
     with private;
   type Component_Access is access all Component'Class;

   Unhandled_Message : exception;

   type Component_Allocation_Class is
     (Auto, Dynamic);

   procedure Set_Allocation_Class
     (C   : in out Component'Class;
      CAC : Component_Allocation_Class);
   --  Set C's allocation class to be CAC.
   --  Its current class must be Auto (the default).

   function Handle_Message
     (C : access Component;
      M : Message'Class)
     return Message'Class
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
     (Port : out Component_Access;
      Target : Component_Access);
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

   procedure Destroy (C : in out Component_Access);
   --  Destroy C.

   -------------------------
   -- Component factories --
   -------------------------

   type Component_Factory is access function
     return Component_Access;

   -------------------------
   -- Group communication --
   -------------------------

   --  Common declarations

   type Group is abstract new Component with private;
   --  A group of target components.

   procedure Subscribe
     (G      : in out Group;
      Target : Component_Access);
   --  Subscribe Target to group G.

   procedure Unsubscribe
     (G      : in out Group;
      Target : Component_Access);
   --  Unsubscribe Target from group G.

   type Multicast_Group is new Group with private;
   --  A group with Multicast semantics: when a message
   --  is received by the group, all subscribers receive it.

   type Anycast_Group is new Group with private;
   --  A group with Anycast semantics: when a message is received
   --  by the group, subscribers receive it sequentially until
   --  one of them handles it.

private

   pragma Inline (Set_Allocation_Class);

   type Null_Message is new Message with null record;

   type Component is
     abstract new Ada.Finalization.Limited_Controlled
     with record
        Allocation_Class : Component_Allocation_Class := Auto;
     end record;

   package Component_Seqs is new PolyORB.Sequences.Unbounded
     (Component_Access);

   subtype Component_Seq is Component_Seqs.Sequence;

   type Group is abstract new Component with record
      Members : Component_Seq;
   end record;

   type Multicast_Group is new Group with null record;

   function Handle_Message
     (Grp : access Multicast_Group;
      Msg : Message'Class)
     return Message'Class;

   type Anycast_Group is new Group with null record;

   function Handle_Message
     (Grp : access Anycast_Group;
      Msg : Message'Class)
     return Message'Class;

end PolyORB.Components;
