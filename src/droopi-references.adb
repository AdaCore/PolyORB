--  Object references.

--  $Id$

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Tags;

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

   function Image (R : Ref) return String
   is
      P : constant Profile_Array
        := Profiles_Of (R);
      Res : Unbounded_String
        := To_Unbounded_String ("Object reference:" & ASCII.LF);
   begin
      for I in P'Range loop
         Res := Res & "  " & Ada.Tags.External_Tag
           (P (I).all'Tag) & ASCII.LF;
         Res := Res & "    " & Binding_Data.Image (P (I).all)
           & ASCII.LF;
      end loop;

      return To_String (Res);
   end Image;

end Droopi.References;
