--  Object references.

--  $Id$

package Droopi.References is

   pragma Preelaborate;

   type Ref is private;
   --  An object reference of any kind.

   function Image (R : Ref) return String;
   --  For debugging purposes.

private

   type Ref is tagged null record;

end Droopi.References;
