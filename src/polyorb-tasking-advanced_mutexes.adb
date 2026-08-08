------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--     P O L Y O R B . T A S K I N G . A D V A N C E D _ M U T E X E S      --
--                                                                          --
--                                 B o d y                                  --
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

--  This package provides an implementation of advanced mutexes.

with PolyORB.Utils.Unchecked_Deallocation;
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
        new PolyORB.Utils.Unchecked_Deallocation.Free

        (Object => Adv_Mutex_Type,

         Name   => Adv_Mutex_Access);
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
