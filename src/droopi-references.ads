--  Object references.

--  $Id$

package Droopi.References is

   pragma Preelaborate;

   type Ref is private;
   --  An object reference of any kind.

private

   type Ref is tagged null record;

end Droopi.References;
