------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--             P O L Y O R B . T A S K I N G . R W _ L O C K S              --
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

--  Inter-process synchronisation objects.

--  $Id: //droopi/main/src/polyorb-tasking-rw_locks.ads#2 $

with PolyORB.Tasking.Condition_Variables;

package PolyORB.Tasking.Rw_Locks is

   pragma Preelaborate;

   ----------------------------
   -- A readers/writers lock --
   ----------------------------

   --  Several tasks can own the lock in R (read) mode.
   --  Only one task can own the lock in W (write) mode.
   --  If there is a request for writing, requests for reading are not accepted
   --  anymore.

   type Rw_Lock_Type is limited private;
   type Rw_Lock_Access is access all Rw_Lock_Type;

   procedure Create (L : out Rw_Lock_Access);
   procedure Destroy (L : in out Rw_Lock_Access);

   procedure Lock_W (L : access Rw_Lock_Type);
   --  Get lock in writing mode.

   procedure Lock_R (L : access Rw_Lock_Type);
   --  Get lock in reading mode.

   procedure Unlock_W (L : access Rw_Lock_Type);
   --  Release writing lock.

   procedure Unlock_R (L : access Rw_Lock_Type);
   --  Release reading mode lock.

   procedure Set_Max_Count
     (L : access Rw_Lock_Type;
      Max : Natural);
   --  Set maximum number of reader.

private

   type Rw_Lock_Type is limited record
      Guard_Values : Tasking.Condition_Variables.Condition_Access;
      --  This condition is signalled each time an attribute
      --  of the Rw_Lock_Type that is used in a guard clause
      --  is changed.

      Readers_Waiting : Natural := 0;
      Writers_Waiting : Natural := 0;

      Serial : Integer := 0;

      Count : Integer := 0;
      --  Current readers, or -1 if held for writing.
      --  If Count >  0, it is the number of tasks owning the lock in R mode.
      --     Count =  0, no tasks own the lock.
      --     Count = -1, a task is owning the lock in W mode.

      Max_Count : Natural := Natural'Last;
      --  Maximum number of readers.
   end record;

end PolyORB.Tasking.Rw_Locks;
