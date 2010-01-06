------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--           P O L Y O R B . T A S K I N G . S E M A P H O R E S            --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2002-2008, Free Software Foundation, Inc.          --
--                                                                          --
-- PolyORB is free software; you  can  redistribute  it and/or modify it    --
-- under terms of the  GNU General Public License as published by the  Free --
-- Software Foundation;  either version 2,  or (at your option)  any  later --
-- version. PolyORB is distributed  in the hope that it will be  useful,    --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License  for more details.  You should have received  a copy of the GNU  --
-- General Public License distributed with PolyORB; see file COPYING. If    --
-- not, write to the Free Software Foundation, 51 Franklin Street, Fifth    --
-- Floor, Boston, MA 02111-1301, USA.                                       --
--                                                                          --
-- As a special exception,  if other files  instantiate  generics from this --
-- unit, or you link  this unit with other files  to produce an executable, --
-- this  unit  does not  by itself cause  the resulting  executable  to  be --
-- covered  by the  GNU  General  Public  License.  This exception does not --
-- however invalidate  any other reasons why  the executable file  might be --
-- covered by the  GNU Public License.                                      --
--                                                                          --
--                  PolyORB is maintained by AdaCore                        --
--                     (email: sales@adacore.com)                           --
--                                                                          --
------------------------------------------------------------------------------

--  This package provides an implementation of counting semaphores.

with Ada.Unchecked_Deallocation;
with PolyORB.Log;

package body PolyORB.Tasking.Semaphores is

   use PolyORB.Log;
   package L is new PolyORB.Log.Facility_Log
     ("polyorb.tasking.semaphores");
   procedure O (Message : String; Level : Log_Level := Debug)
     renames L.Output;
   function C (Level : Log_Level := Debug) return Boolean
     renames L.Enabled;

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
      pragma Debug (C, O ("Create"));

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
      pragma Debug (C, O ("Destroy semaphore, Value was "
                       & Integer'Image (S.Value)));

      PTM.Destroy (S.Mutex);
      PTCV.Destroy (S.Condition);
      Free (S);
   end Destroy;

   -------
   -- V --
   -------

   procedure V (S : Semaphore_Access) is
   begin
      PTM.Enter (S.Mutex);

      pragma Debug (C, O ("V (sem), value ="
                       & Integer'Image (S.Value)));

      S.Value := S.Value + 1;
      PTCV.Signal (S.Condition);
      PTM.Leave (S.Mutex);
   end V;

   -------
   -- P --
   --------

   procedure P (S : Semaphore_Access) is
   begin
      PTM.Enter (S.Mutex);
      pragma Debug (C, O ("P (sem)"));

      while S.Value = 0 loop
         pragma Debug (C, O ("Value is null, wait in semaphore"));
         PTCV.Wait (S.Condition, S.Mutex);
      end loop;

      S.Value := S.Value - 1;

      PTM.Leave (S.Mutex);
   end P;

   -----------
   -- State --
   -----------

   function State (S : Semaphore_Access) return Natural is
      Result : Integer;
   begin
      PTM.Enter (S.Mutex);
      Result := S.Value;

      pragma Debug (C, O ("Get Semaphore value, value ="
                       & Integer'Image (S.Value)));

      PTM.Leave (S.Mutex);
      return Result;
   end State;

end PolyORB.Tasking.Semaphores;
