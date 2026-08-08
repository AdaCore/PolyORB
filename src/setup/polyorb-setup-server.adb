------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                 P O L Y O R B . S E T U P . S E R V E R                  --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2001-2012, Free Software Foundation, Inc.          --
--                                                                          --
-- This is free software;  you can redistribute it  and/or modify it  under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  This software is distributed in the hope  that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License for  more details.                                               --
--                                                                          --
-- As a special exception under Section 7 of GPL version 3, you are granted --
-- additional permissions described in the GCC Runtime Library Exception,   --
-- version 3.1, as published by the Free Software Foundation.               --
--                                                                          --
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
--                                                                          --
--                  PolyORB is maintained by AdaCore                        --
--                     (email: sales@adacore.com)                           --
--                                                                          --
------------------------------------------------------------------------------

--  Set up a simple ORB to act as a server

--  The user must take care of also setting up a tasking runtime and a
--  ORB tasking policy.

pragma Style_Checks ("M2048");
--  Configure substitutions may yield long lines
--  src/setup/polyorb-setup-server.adb.  Generated from polyorb-setup-server.adb.in by configure.

with PolyORB.Setup.Base;
pragma Warnings (Off, PolyORB.Setup.Base);
pragma Elaborate_All (PolyORB.Setup.Base);

with PolyORB.Setup.OA.Basic_POA;
pragma Warnings (Off, PolyORB.Setup.OA.Basic_POA);
pragma Elaborate_All (PolyORB.Setup.OA.Basic_POA);

--  Personalities setup
----------
-- GIOP --
----------

--  with PolyORB.Setup.SSLIOP;
--  pragma Elaborate_All (PolyORB.Setup.SSLIOP);
--  pragma Warnings (Off, PolyORB.Setup.SSLIOP);

with PolyORB.Setup.IIOP;
pragma Elaborate_All (PolyORB.Setup.IIOP);
pragma Warnings (Off, PolyORB.Setup.IIOP);

with PolyORB.Setup.UIPMC;
pragma Elaborate_All (PolyORB.Setup.UIPMC);
pragma Warnings (Off, PolyORB.Setup.UIPMC);

with PolyORB.Setup.DIOP;
pragma Elaborate_All (PolyORB.Setup.DIOP);
pragma Warnings (Off, PolyORB.Setup.DIOP);

with PolyORB.Setup.Access_Points.IIOP;
pragma Elaborate_All (PolyORB.Setup.Access_Points.IIOP);
pragma Warnings (Off, PolyORB.Setup.Access_Points.IIOP);

--  with PolyORB.Setup.Access_Points.SSLIOP;
--  pragma Elaborate_All (PolyORB.Setup.Access_Points.SSLIOP);
--  pragma Warnings (Off, PolyORB.Setup.Access_Points.SSLIOP);

with PolyORB.Setup.Access_Points.UIPMC;
pragma Elaborate_All (PolyORB.Setup.Access_Points.UIPMC);
pragma Warnings (Off, PolyORB.Setup.Access_Points.UIPMC);

with PolyORB.Setup.Access_Points.DIOP;
pragma Elaborate_All (PolyORB.Setup.Access_Points.DIOP);
pragma Warnings (Off, PolyORB.Setup.Access_Points.DIOP);

package body PolyORB.Setup.Server is
end PolyORB.Setup.Server;
