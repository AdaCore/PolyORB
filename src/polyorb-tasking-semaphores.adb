------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--           P O L Y O R B - T A S K I N G - S E M A P H O R E S            --
--                                                                          --
--                                 B o d y                                  --
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

with Ada.Unchecked_Deallocation;

package body PolyORB.Tasking.Semaphores is

   ----------
   -- Free --
   ----------

   procedure Free is new Ada.Unchecked_Deallocation
     (Semaphore, Semaphore_Access);

   ------------
   -- Create --
   ------------

   procedure Create (S : out Semaphore_Access) is
      S                     : Semaphore_Type;
      The_Mutex_Factory     : PTM.Mutex_Factory := PTM.Get_Mutex_Factory;
      The_Condition_Factory : PTCV.Condition_Factory
        := PTM.Get_Condition_Factory;
   begin
      S.Value := 0;
      S.Mutex := PTM.Create (The_Mutex_Factory);
      S.Condition := PTCV.Create (The_Condition_Factory);
      return new Semaphore'(S);
   end Create;

   procedure Destroy (S : in out Semaphore_Access) is
      The_Mutex_Factory     : PTM.Mutex_Factory := PTM.Get_Mutex_Factory;
      The_Condition_Factory : PTCV.Condition_Factory
        := PTM.Get_Condition_Factory;
   begin
      PTM.Destroy (The_Mutex_Factory, S.Mutex);
      PTCV.Destroy (The_Condition_Factory, S.Condition);
      Free (S);
   end Destroy;

   procedure Up (S : Semaphore_Access) is
   begin
      Enter (M.Mutex.all);
      S.Value := S.Value + 1;
      PTCV.Signal (M.Condition_Variables);
      Leave (M.Mutex.all);
   end Up;

   procedure Down (S : Semaphore_Access) is
   begin
      Enter (M.Mutex.all);

      while Value = 0 loop
         Wait (M.Condition.all, M.Mutex);
      end loop;
      Value := Value - 1;

      Leave (M.Mutex.all);
   end Down;

   function State (S : Semaphore_Access) return Natural is
      Result : Integer;
   begin
      Enter (M.Mutex.all);
      Result := S.Value;
      Leave (M.Mutex.all);
      return Result;
   end State;

end PolyORB.Tasking.Semaphores;
