--  Object references.

--  $Id$

package body Droopi.References is

   procedure Create_Reference
     (Profiles : Profile_Array;
      R        : out Ref) is
   begin
      if Profiles'Length = 0 then
         R := (Nil_Ref => True);
      else
         R := (Nil_Ref  => False,
               Profiles => Profile_Seqs.To_Sequence (Profiles));
      end if;
   end Create_Reference;

   function Profiles_Of (R : Ref) return Profile_Array is
   begin
      return Profile_Seqs.To_Element_Array (R.Profiles);
   end Profiles_Of;

   function Is_Nil (R : Ref) return Boolean is
   begin
      return R.Nil_Ref;
   end Is_Nil;

   function Image (R : Ref) return String is
   begin
      --  XXX TODO!
      return "<ObjectRef>";
   end Image;

end Droopi.References;
