------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                      P O L Y O R B . T A S K I N G                       --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--         Copyright (C) 2002-2012, Free Software Foundation, Inc.          --
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

--  The children of this package provides soft links for the tasking.

--  Coding guidelines for children of this package:
--  -----------------------------------------------
--  In order to compile in High Integrity mode, it should not use:
--  * exception handlers;
--  * string concatenation (&);
--  * outputs;
--  * implicit loops (for example, the initialisation of an array must use
--    an explicit loop);
--  * controled types.
--  Dynamic allocation should be avoided; they should either be placed
--  in profile specific parts (if the profile allows it), or it should
--  be the responsability of the client of these packages.

--  As a special exception, some allocation and initialization for
--  these packages can be done at elaboration. They should be minimal.

package PolyORB.Tasking is

   pragma Pure;

end PolyORB.Tasking;
