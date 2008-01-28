------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--     P O L Y O R B . T A S K I N G . A D V A N C E D _ M U T E X E S      --
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

--  This package provides an implementation of advanced mutexes.

with Ada.Unchecked_Deallocation;
with PolyORB.Log;

package body PolyORB.Tasking.Advanced_Mutexes is

   use PolyORB.Log;

   package PTT renames PolyORB.Tasking.Threads;

   package L is new PolyORB.Log.Facility_Log
     ("polyorb.tasking.advanced_mutexes");
   procedure O (Message : String; Level : Log_Level := Debug)
     renames L.Output;
   function C (Level : Log_Level := Debug) return Boolean
     renames L.Enabled;

   ------------
   -- Create --
   ------------

   procedure Create (M : out Adv_Mutex_Access) is
      use PolyORB.Tasking.Threads;
   begin
      pragma Debug (C, O ("Create"));

      M := new Adv_Mutex_Type;
      M.Current := Current_Task;
      M.Empty := True;
      PTM.Create (M.MMutex);
      PTCV.Create (M.MCondition);
      M.Level := 0;
      M.Await_Count := 0;
      M.Passing := True;
   end Create;

   -------------
   -- Destroy --
   -------------

   procedure Destroy (M : in out Adv_Mutex_Access)
   is
      procedure Free is
        new Ada.Unchecked_Deallocation (Adv_Mutex_Type, Adv_Mutex_Access);
   begin
      pragma Debug (C, O ("Destroy"));

      PTM.Destroy (M.MMutex);
      PTCV.Destroy (M.MCondition);
      Free (M);
   end Destroy;

   -----------
   -- Enter --
   -----------

   procedure Enter (M : access Adv_Mutex_Type) is
      use PolyORB.Tasking.Threads;
      Self : constant Thread_Id := Current_Task;
   begin
      PTM.Enter (M.MMutex);

      pragma Debug (C, O (PTT.Image (Self)
                       & " tries to Enter Adv_Mutex"));

      while not M.Empty and then M.Current /= Self loop

         pragma Debug (C, O (PTT.Image (Self)
                          & " will wait for Adv_Mutex, current owner is "
                          & PTT.Image (M.Current)));

         if not M.Passing then
            PTCV.Wait (M.MCondition, M.MMutex);
         end if;

         M.Passing := False;

      end loop;

      M.Empty := False;
      M.Level := M.Level + 1;
      M.Current := Self;

      pragma Debug (C, O ("Enter: " & PTT.Image (M.Current)));
      pragma Debug (C, O (" new level:" & Integer'Image (M.Level)));
      PTM.Leave (M.MMutex);
   end Enter;

   -----------
   -- Leave --
   -----------

   procedure Leave (M : access Adv_Mutex_Type) is
      use PolyORB.Tasking.Threads;
      Self : constant Thread_Id := Current_Task;
   begin
      PTM.Enter (M.MMutex);

      pragma Debug (C, O ("Leave, owner was "
                       & PTT.Image (Self)));

      pragma Assert (M.Current = Self);
      pragma Assert (M.Level > 0);

      M.Level := M.Level - 1;

      if M.Level = 0 then
         M.Empty := True;
         M.Passing := True;
         PTCV.Signal (M.MCondition);
      end if;

      pragma Debug (C, O (" new level:" & Integer'Image (M.Level)));
      PTM.Leave (M.MMutex);
   end Leave;

end PolyORB.Tasking.Advanced_Mutexes;
