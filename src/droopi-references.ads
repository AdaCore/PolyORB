--  Object references.

--  $Id$

with Droopi.Objects;

package Droopi.References is

   pragma Elaborate_Body;

   type Ref is private;
   --  An object reference of any kind.

   function Image (R : Ref) return String;
   --  For debugging purposes.

   function Bind (R : Ref) return Objects.Servant_Access;

   Nil_Ref : constant Ref;

private

   type Ref is tagged null record;

   Nil_Ref : constant Ref := (null record);

end Droopi.References;
