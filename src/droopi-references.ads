--  Object references.

--  $Id$

package Droopi.References is

   pragma Preelaborate;

   type Ref is private;
   --  An object reference of any kind.

   function Image (R : Ref) return String;
   --  For debugging purposes.

   Nil_Ref : constant Ref;

private

   type Ref is tagged null record;

   Nil_Ref : constant Ref := (null record);

end Droopi.References;
