--  Initialization of a DROOPI server.
--  This package has several child procedures, corresponding
--  to different compile-time configurations.

--  $Id$

with Droopi.ORB;

package Droopi.Setup is

   --  No proper body: no need for elab control pragma.

   The_ORB : Droopi.ORB.ORB_Access;

end Droopi.Setup;
