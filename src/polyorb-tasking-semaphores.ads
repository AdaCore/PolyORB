------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--           P O L Y O R B . T A S K I N G . S E M A P H O R E S            --
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

--  This package provides an implementation of counting semaphores.

with PolyORB.Tasking.Mutexes;
with PolyORB.Tasking.Condition_Variables;

package PolyORB.Tasking.Semaphores is

   pragma Preelaborate;

   type Semaphore is private;
   type Semaphore_Access is access all Semaphore;

   procedure Create (S : out Semaphore_Access);

   procedure Destroy (S : in out Semaphore_Access);

   procedure V (S : Semaphore_Access);
   --  V-operation on the semaphore: increment the value of the
   --  semaphore in a thread-safe way.

   procedure P (S : Semaphore_Access);
   --  P-operation on the semaphore: block until S.Value > 0; then
   --  decrement the value in a thread-safe way.

   function State (S : Semaphore_Access) return Natural;
   --  Return the current value of the semaphore.

private

   package PTM renames PolyORB.Tasking.Mutexes;
   package PTCV renames PolyORB.Tasking.Condition_Variables;

   type Semaphore is record
      Value     : Natural;
      --  Current value of the semaphore.

      Mutex     : PTM.Mutex_Access;
      --  Used to assure mutual exclusion for Up, Down and State.

      Condition : PTCV.Condition_Access;
      --  Used the implement the blocking call to Down.
   end record;

end PolyORB.Tasking.Semaphores;
