--  Object references.

--  $Id$

with Sequences.Unbounded;

with Droopi.Binding_Data;

package Droopi.References is

   pragma Elaborate_Body;

   type Ref is private;
   --  An object reference of any kind.

   function Is_Nil (R : Ref) return Boolean;

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
      end case;
   end record;

end Droopi.References;
