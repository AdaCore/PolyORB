--  Object references.

--  $Id$

package body Droopi.References is

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
