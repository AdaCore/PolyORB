------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                   P O L Y O R B . S E M A P H O R E S                    --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                Copyright (C) 2001 Free Software Fundation                --
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
--  This package provides an implementation of semaphores

--  $Id$

with PolyORB.Soft_Links;

package PolyORB.Utils.Semaphores is

   pragma Preelaborate;

   type Semaphore is private;

   type Semaphore_Access is access all Semaphore;

   procedure Create (S : out Semaphore_Access);

   procedure Up (S : Semaphore_Access);

   procedure Down (S : Semaphore_Access);

   function State (S : Semaphore_Access) return Natural;

private

   use PolyORB.Soft_Links;

   type Semaphore is record
      N             : Natural;
      Is_Empty      : Mutex_Access;
      Down_Lock     : Mutex_Access;
      Full_Lock     : Mutex_Access;
   end record;

end PolyORB.Utils.Semaphores;
