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
--        P := Find_Best_Profile (R);
--        if Tag (P) = Tag_Local then
--           return P.Object;
--        else
--           S := Find_Session (P.Address);
--           return Make_Surrogate (S);
--        end if;

      --  XXX TODO!
      raise Not_Implemented;
      pragma Warnings (Off, Bind);
      --  XXX return is never reached.
      return null;
   end Bind;

end Droopi.References;
