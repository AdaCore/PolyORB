--  Object references.

--  $Id$

with Sequences.Unbounded;

with Droopi.Binding_Data;

package Droopi.References is

   pragma Elaborate_Body;

   type Ref is private;
   --  An object reference of any kind.

   function Is_Nil (R : Ref) return Boolean;
   --  True iff R is a Nil reference, i.e. a reference that
   --  does not designate any object.

   function Image (R : Ref) return String;
   --  For debugging purposes.

private

   package Profile_Seqs is new Sequences.Unbounded
     (Binding_Data.Profile_Access);
   subtype Profile_Seq is Profile_Seqs.Sequence;

   type Ref (Nil_Ref : Boolean := True) is record
      case Nil_Ref is
         when True =>
            null;
         when False =>
            Profiles : Profile_Seq;
            --  The collection of tagged profiles that designate
            --  transport access points where this object can be
            --  contacted, together with the object ids to be used.
      end case;
   end record;

end Droopi.References;
