--  Support the addition of external information ("annotations")
--  to objects by their client. The object does not need to have
--  visibility on the client in order to allow itself to be annotated;
--  it only needs to expose a Notepad attribute.

--  $Id$

with Sequences.Unbounded;

package Droopi.Annotations is

   type Note is abstract tagged private;
   --  A note that can be attached to an object.

   type Notepad is private;
   type Notepad_Access is access all Notepad;
   --  A space for clients of an object to attach Notes into.
   --  Notepad_Access can be used by private types to selectively
   --  expose one Notepad component to their clients.

   procedure Set_Note (NP : in out Notepad; N : Note'Class);
   --  Add note N to notepad NP. If of the same type already
   --  exists, it is replaced by N.

   procedure Get_Note (NP : Notepad; N : out Note'Class);
   --  Retrieve a note of N's type from NP.

   procedure Destroy (NP : in out Notepad);
   --  Removes all notes in NP and return any associated
   --  resources to the system.

private

   type Note is abstract tagged null record;
   type Note_Access is access all Note'Class;

   package Note_Seqs is new Sequences.Unbounded (Note_Access);
   subtype Note_Seq is Note_Seqs.Sequence;

   type Notepad is new Note_Seqs.Sequence;
   --  Cannot be declared as "type Notepad is new Note_Seq;"
   --  because this would be a derivation of a partial view
   --  whose full view is tagged within its immediate scope,
   --  which is illegal.

end Droopi.Annotations;
