--  Support the addition of external information ("annotations")
--  to objects by their client. The object does not need to have
--  visibility on the client in order to allow itself to be annotated;
--  it only needs to expose a Notepad attribute.

--  $Id$

with Ada.Tags;
with Ada.Unchecked_Deallocation;

package body Droopi.Annotations is

   use Ada.Tags;

   procedure Set_Note (NP : in out Notepad; N : Note'Class)
   is
      The_Notes : constant Note_Seqs.Element_Array
        := Note_Seqs.To_Element_Array (Note_Seq (NP));
   begin
      for I in The_Notes'Range loop
         if The_Notes (I)'Tag = N'Tag then
            The_Notes (I).all := N;
            --  Here we have checked that The_Notes (I).all and N
            --  are of the same type, but this does not guarantee
            --  that the assignment will succeed: Constraint_Error
            --  may be raised if that type has known discriminants
            --  without default values.
            return;
         end if;
      end loop;

      Note_Seqs.Append (Note_Seq (NP), new Note'Class'(N));
   end Set_Note;

   procedure Get_Note (NP : Notepad; N : out Note'Class)
   is
      The_Notes : constant Note_Seqs.Element_Array
        := Note_Seqs.To_Element_Array (Note_Seq (NP));
   begin
      for I in The_Notes'Range loop
         if The_Notes (I)'Tag = N'Tag then
            N := The_Notes (I).all;
            return;
         end if;
      end loop;
   end Get_Note;

   procedure Destroy (NP : in out Notepad)
   is
      The_Notes : Note_Seqs.Element_Array
        := Note_Seqs.To_Element_Array (Note_Seq (NP));

      procedure Free is new Ada.Unchecked_Deallocation
        (Note'Class, Note_Access);

   begin
      for I in The_Notes'Range loop
         Free (The_Notes (I));
      end loop;

      NP := Notepad (Note_Seqs.Null_Sequence);
   end Destroy;

end Droopi.Annotations;
