--  Object references.

--  $Id$

package body Droopi.References is

   function Image (R : Ref) return String is
   begin
      --  XXX TODO!
      return "<ObjectRef>";
   end Image;

   function Bind (R : Ref) return Objects.Servant_Access is
   begin
      --  XXX TODO!
      raise Not_Implemented;
      pragma Warnings (Off, Bind);
      --  XXX return is never reached.
      return null;
   end Bind;

end Droopi.References;
