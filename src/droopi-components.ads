--  A communication filter (a transport Data_Unit handler/forwarder).

--  $Id$

with Sequences.Unbounded;

package Droopi.Components is

   -------------------------------------
   -- Abstract message and components --
   -------------------------------------

   type Message is abstract tagged private;

   type Component is abstract tagged limited private;
   type Component_Access is access all Component'Class;

   function Handle_Message
     (C : Component;
      M : Message'Class)
     return Boolean
      is abstract;
   --  Called when component C is to receive message M.
   --  Return True if M has been handled, false otherwise.

   procedure Connect
     (Signal : out Component_Access;
      Target : Component_Access);
   --  Connect Signal to Target: when Signal is emitted with message
   --  M, Target receives M.

   procedure Emit
     (Signal : Component_Access;
      Msg    : Message'Class);
   --  Emit message Msg on Signal.

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

   type Message is abstract tagged null record;

   type Component is abstract tagged limited null record;

   package Component_Seqs is new Sequences.Unbounded
     (Component_Access);
   subtype Component_Seq is Component_Seqs.Sequence;

   type Group is abstract new Component with record
      Members : Component_Seq;
   end record;

   type Multicast_Group is new Group with null record;

   function Handle_Message
     (Grp : Multicast_Group;
      Msg : Message'Class)
     return Boolean;

   type Anycast_Group is new Group with null record;

   function Handle_Message
     (Grp : Anycast_Group;
      Msg : Message'Class)
     return Boolean;

end Droopi.Components;
