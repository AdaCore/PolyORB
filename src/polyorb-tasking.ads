------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                      P O L Y O R B . T A S K I N G                       --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--             Copyright (C) 1999-2002 Free Software Fundation              --
--                                                                          --
-- PolyORB is free software; you  can  redistribute  it and/or modify it    --
-- under terms of the  GNU General Public License as published by the  Free --
-- Software Foundation;  either version 2,  or (at your option)  any  later --
-- version. PolyORB is distributed  in the hope that it will be  useful,    --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License  for more details.  You should have received  a copy of the GNU  --
-- General Public License distributed with PolyORB; see file COPYING. If    --
-- not, write to the Free Software Foundation, 59 Temple Place - Suite 330, --
-- Boston, MA 02111-1307, USA.                                              --
--                                                                          --
-- As a special exception,  if other files  instantiate  generics from this --
-- unit, or you link  this unit with other files  to produce an executable, --
-- this  unit  does not  by itself cause  the resulting  executable  to  be --
-- covered  by the  GNU  General  Public  License.  This exception does not --
-- however invalidate  any other reasons why  the executable file  might be --
-- covered by the  GNU Public License.                                      --
--                                                                          --
--              PolyORB is maintained by ENST Paris University.             --
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

--  $Id$

package PolyORB.Tasking is
   pragma Pure;

   Tasking_Error : exception;
   --  The functionalities of PolyORB.Tasking's children must only be
   --  used by tasks that have been created through this API.  If this
   --  functionalities are used by tasks that have not been created
   --  through it, a Tasking_Error is raised.

   Tasking_Profile_Error : exception;
   --  Some functionnalities may have no sense for some profile.
   --  In that case, any call to this functionnalities raise
   --  a Tasking_Profile_Error.

end PolyORB.Tasking;
