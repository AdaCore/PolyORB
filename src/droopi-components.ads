--  Abstract components communicating through synchronous messages.

--  $Id$

with Sequences.Unbounded;

package Droopi.Components is

   -------------------------------------
   -- Abstract message and components --
   -------------------------------------

   type Message is abstract tagged null record;

   type Null_Message is new Message with private;

   type Component is abstract tagged limited private;
   type Component_Access is access all Component'Class;

   Unhandled_Message : exception;

   function Handle_Message
     (C : access Component;
      M : Message'Class)
     return Message'Class
      is abstract;
   --  Called when component C is to receive message M.
   --  Return True if M has been handled, false otherwise.

   procedure Connect
     (Port : out Component_Access;
      Target : Component_Access);
   --  Connect Port to Target: when Port is emitted with message
   --  M, Target receives M.

   function Emit
     (Port : Component_Access;
      Msg    : Message'Class)
     return Message'Class;
   --  Emit message Msg on Port.

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

   type Null_Message is new Message with null record;

   type Component is abstract tagged limited null record;

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
