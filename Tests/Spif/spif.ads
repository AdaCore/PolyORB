--
--  $Id$
--

package Spif is

   --  This package is the root of a tree containing Spif/Ada low-level
   --  interfaces. These interfaces do add almost no code to Spif's kernel
   --  so it is probably worth letting it in in any situation.
   --  Packages under the Spif hierarchy do *NOT* take care of any
   --  locking of any kind. The user must ensure that a proper locking
   --  policy is enforced to prevent concurrent access to unprotected
   --  structures.

   pragma Pure;

end Spif;
