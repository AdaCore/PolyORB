------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--   P O L Y O R B . T A S K I N G . P R O F I L E S . R A V E N S C A R    --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--         Copyright (C) 2002-2003 Free Software Foundation, Inc.           --
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
--                PolyORB is maintained by ACT Europe.                      --
--                    (email: sales@act-europe.fr)                          --
--                                                                          --
------------------------------------------------------------------------------

--  Base package for the Ravenscar implementation of PolyORB.Tasking.
--  This children of this package must compile under the Ravenscar profile,
--  and must avoid to use dynamic allocation. If dynamic allocation cannot
--  be avoided, it must be done at initialisation time.

--  $Id$

package PolyORB.Tasking.Profiles.Ravenscar is

   pragma Preelaborate;

   Storage_Size : constant := 262_144;
   --  Stack size of the tasks of the pool.
   --  WAG:3.15
   --  In 3.15, the pragma Ravenscar implies the restriction
   --  Static_Storage_Size, so we cannot make Storage_Size a formal
   --  generic parameter of PTPR.Threads.
   --  This restriction has been removed in 3.16,

end PolyORB.Tasking.Profiles.Ravenscar;
