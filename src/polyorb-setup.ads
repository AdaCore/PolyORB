--  Initialization of a PolyORB server.
--  This package has several child procedures, corresponding
--  to different compile-time configurations.

--  $Id$

with PolyORB.ORB;

package PolyORB.Setup is

   --  No proper body: no need for elab control pragma.

   The_ORB : PolyORB.ORB.ORB_Access;

end PolyORB.Setup;
