--  Abstract components communicating through synchronous messages.

--  $Id$

with Ada.Finalization;

with Sequences.Unbounded;
pragma Elaborate_All (Sequences.Unbounded);

package Droopi.Components is

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
   --  Return True if M has been handled, false otherwise.
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

   package Component_Seqs is new Sequences.Unbounded
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

end Droopi.Components;
