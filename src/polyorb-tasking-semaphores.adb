------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--           P O L Y O R B . T A S K I N G . S E M A P H O R E S            --
--                                                                          --
--                                 B o d y                                  --
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

--  This package provides an implementation of semaphores

--  $Id$

with Ada.Unchecked_Deallocation;
with PolyORB.Log;

package body PolyORB.Tasking.Semaphores is

   use PolyORB.Log;

   package L is new PolyORB.Log.Facility_Log
     ("polyorb.tasking.semaphores");

   procedure O (Message : in String; Level : Log_Level := Debug)
     renames L.Output;

   ----------
   -- Free --
   ----------

   procedure Free is new Ada.Unchecked_Deallocation
     (Semaphore, Semaphore_Access);

   ------------
   -- Create --
   ------------

   procedure Create (S : out Semaphore_Access) is
   begin
      pragma Debug (O ("Create"));

      S := new Semaphore;
      S.Value := 0;
      PTM.Create (S.Mutex);
      PTCV.Create (S.Condition);
   end Create;

   -------------
   -- Destroy --
   -------------

   procedure Destroy (S : in out Semaphore_Access) is
   begin
      pragma Debug (O ("Destroy semaphore, Value was "
                       & Integer'Image (S.Value)));

      PTM.Destroy (S.Mutex);
      PTCV.Destroy (S.Condition);
      Free (S);
   end Destroy;

   --------
   -- Up --
   --------

   procedure Up (S : Semaphore_Access) is
   begin
      PTM.Enter (S.Mutex);

      pragma Debug (O ("Up semaphore, value ="
                       & Integer'Image (S.Value)));

      S.Value := S.Value + 1;
      PTCV.Signal (S.Condition);
      PTM.Leave (S.Mutex);
   end Up;

   ----------
   -- Down --
   ----------

   procedure Down (S : Semaphore_Access) is
   begin
      PTM.Enter (S.Mutex);
      pragma Debug (O ("Down semaphore"));

      while S.Value = 0 loop
         pragma Debug (O ("Wait in Semaphore, value ="
                          & Integer'Image (S.Value)));
         PTCV.Wait (S.Condition, S.Mutex);
      end loop;
      S.Value := S.Value - 1;

      PTM.Leave (S.Mutex);
   end Down;

   -----------
   -- State --
   -----------

   function State (S : Semaphore_Access) return Natural is
      Result : Integer;
   begin
      PTM.Enter (S.Mutex);
      Result := S.Value;

      pragma Debug (O ("Get Semaphore value, value ="
                       & Integer'Image (S.Value)));

      PTM.Leave (S.Mutex);
      return Result;
   end State;

end PolyORB.Tasking.Semaphores;
