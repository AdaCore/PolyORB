------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--             P O L Y O R B . T A S K I N G . P R O F I L E S              --
--       . R A V E N S C A R . C O N D I T I O N _ V A R I A B L E S        --
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

--  Implementation of synchronisation objects under the ravenscar profile.

with PolyORB.Tasking.Profiles.Ravenscar.Threads;

with PolyORB.Initialization;
with PolyORB.Utils.Strings;
with PolyORB.Log;

package body PolyORB.Tasking.Profiles.Ravenscar.Condition_Variables is

   use PolyORB.Log;

   package PTM renames PolyORB.Tasking.Mutexes;

   package PTCV renames PolyORB.Tasking.Condition_Variables;

   package L is new PolyORB.Log.Facility_Log
     ("polyorb.tasking.profiles.ravenscar.condition_variables");

   procedure O (Message : in String; Level : Log_Level := Debug)
     renames L.Output;

   type Queued_Thread is record
      --  Element of a queue of Thread; see comment for Thread_Queue.
      Sync       : Synchro_Index_Type;
      --  Synchro object the Thread is waiting on.

      Next       : Extended_Synchro_Index;
      --  Next Thread in the queue.

      Is_Waiting : Boolean;
      --  True if the thread is waiting.

   end record;

   type Thread_Queue is array (Synchro_Index_Type) of Queued_Thread;
   --  Implementation of a queue using an array.
   --  Each element of the array represent a waiting thread, and
   --  contain an access to the synchro object on which it is waiting,
   --  and the index of the Thread following it in the queue.
   --  This queue  is used by a condition variable to record the tasks that
   --  wait for it.
   --  The place of a Thread in the array change at every suspending call;
   --  It is determinate by the index of its current synchro object.

   type Condition_Pool_Type is array (Condition_Index_Type)
     of aliased Ravenscar_Condition_Type;

   The_Condition_Pool : Condition_Pool_Type;
   --  The pool of preallocated condition variables.

   protected type Condition_PO is
      --  Provide thread safe primitives for a  Mutex,
      --  and manage its Thread_Queue.

      function Check_Queue_Consistency return Boolean;
      --  Function supposed to be used in an assert statement.
      --  It check some simple properties of the Thread_Queue :
      --  No loop, no error in the Is_Waiting flags...

      procedure Prepare_Wait
        (S : Synchro_Index_Type);
      --  Inform the PO that the current task is about to Wait.

      procedure Signal
        (Someone_Is_Waiting : out Boolean;
         To_Free            : out Synchro_Index_Type);
      --  Protected part of the implementation of Signal.

      procedure Broadcast (To_Free : out Thread_Queue);
      --  Protected part of the implementation Broadcast.

      procedure Initialize (N : Condition_Index_Type);
      --  Initialize the condition variable.

   private
      My_Index         : Condition_Index_Type;
      First            : Extended_Synchro_Index;
      Waiters          : Thread_Queue;
   end Condition_PO;

   type Condition_PO_Arr is array (Condition_Index_Manager.Index_Type)
     of Condition_PO;

   The_Condition_PO_Arr : Condition_PO_Arr;
   --  Pool of Condition_PO.

   ---------------
   -- Broadcast --
   ---------------

   procedure Broadcast
     (C : in out Ravenscar_Condition_Type) is
      To_Free : Thread_Queue;
   begin
      pragma Debug (O ("Broadcast"));
      The_Condition_PO_Arr (C.Id).Broadcast (To_Free);
      for J in To_Free'Range loop

         if To_Free (J).Is_Waiting = True then
            Resume (To_Free (J).Sync);
         end if;

      end loop;
   end Broadcast;

   ------------
   -- Create --
   ------------

   function Create
     (MF   : access Ravenscar_Condition_Factory_Type;
      Name : String := "")
     return PTCV.Condition_Access is
      pragma Warnings (Off);
      pragma Unreferenced (MF);
      pragma Unreferenced (Name);
      pragma Warnings (On);
      --  XXX The use of names is not implemented yet.
      Index : Condition_Index_Type;
      C     : Ravenscar_Condition_Access;
   begin
      pragma Debug (O ("Create"));
      Condition_Index_Manager.Get (Index);
      C := The_Condition_Pool (Index)'Access;
      C.Id := Index;
      The_Condition_PO_Arr (C.Id).Initialize (C.Id);
      return Condition_Access (C);
   end Create;

   -------------
   -- Destroy --
   -------------

   procedure Destroy
     (MF : in out Ravenscar_Condition_Factory_Type;
      C  : in out Condition_Access) is
      pragma Warnings (Off);
      pragma Unreferenced (MF);
      pragma Warnings (On);
   begin
      pragma Debug (O ("Destroy"));
      Condition_Index_Manager.Release (Ravenscar_Condition_Access (C).Id);
   end Destroy;

   ------------------
   -- Condition_PO --
   ------------------

   protected body Condition_PO is
      --  XXX gestion of the queue not implemented yet.

      ------------------------------------------
      -- Condition_PO.Check_Queue_Consistency --
      ------------------------------------------

      function Check_Queue_Consistency return Boolean is
         type Bool_Arr is array (Waiters'Range) of Boolean;
         Marked  : Bool_Arr;
         Current : Extended_Synchro_Index := First;
      begin
         for J in Marked'Range loop
            Marked (J) := False;
         end loop;
         while Current /= Null_Synchro_Index loop

            if Marked (Synchro_Index_Type (Current)) then
               --  Loop in the queue
               return False;
            end if;

            if not Waiters (Synchro_Index_Type (Current)).Is_Waiting then
               --  Someone is in the queue, but does not wait.
               return False;
            end if;

            Marked (Synchro_Index_Type (Current)) := True;
            Current := Waiters (Synchro_Index_Type (Current)).Next;
         end loop;

         return True;
      end Check_Queue_Consistency;

      ----------------------------
      -- Condition_PO.Broadcast --
      ----------------------------

      procedure Broadcast (To_Free : out Thread_Queue) is
      begin
         pragma Assert (Check_Queue_Consistency);
         To_Free := Waiters;
         First := Null_Synchro_Index;
         for J in Waiters'Range loop
            Waiters (J).Is_Waiting := False;
         end loop;
         pragma Assert (Check_Queue_Consistency);
      end Broadcast;

      -----------------------------
      -- Condition_PO.Initialize --
      -----------------------------

      procedure Initialize (N : Condition_Index_Type) is
      begin
         My_Index := N;
         First := Null_Synchro_Index;
         for J in Waiters'Range loop
            Waiters (J).Next := Null_Synchro_Index;
            Waiters (J).Is_Waiting := False;
         end loop;
         pragma Assert (Check_Queue_Consistency);
      end Initialize;

      -------------------------------
      -- Condition_PO.Prepare_Wait --
      -------------------------------

      procedure Prepare_Wait
        (S : Synchro_Index_Type) is
         Current   : Extended_Synchro_Index;
         Precedent : Extended_Synchro_Index;
      begin
         pragma Assert (Check_Queue_Consistency);
         Waiters (S).Is_Waiting := True;
         Waiters (S).Sync := S;

         if First /= Null_Synchro_Index then
            --  This loop search the rank of T in the queue:
            Current := Waiters (Synchro_Index_Type (First)).Next;
            Precedent := First;
            while Current /= Null_Synchro_Index loop
               --  XXX compare the Priorities...
               Precedent := Current;
               Current := Waiters (Synchro_Index_Type (Current)).Next;
            end loop;

            Waiters (Synchro_Index_Type (Precedent)).Next
              := Extended_Synchro_Index (S);
            Waiters (S).Next := Current;

         else
            First := Extended_Synchro_Index (S);
            Waiters (S).Next := Null_Synchro_Index;
         end if;
         pragma Assert (Check_Queue_Consistency);
      end Prepare_Wait;

      -------------------------
      -- Condition_PO.Signal --
      -------------------------

      procedure Signal
        (Someone_Is_Waiting : out Boolean;
         To_Free            : out Synchro_Index_Type) is
         Former_First : constant Extended_Synchro_Index := First;
      begin
         pragma Assert (Check_Queue_Consistency);
         Someone_Is_Waiting := First /= Null_Synchro_Index;

         if Someone_Is_Waiting then
            First := Waiters (Synchro_Index_Type (Former_First)).Next;
            Waiters (Synchro_Index_Type (Former_First)).Next
              := Null_Synchro_Index;
            Waiters (Synchro_Index_Type (Former_First)).Is_Waiting := False;
            To_Free := Waiters (Synchro_Index_Type (Former_First)).Sync;
         end if;

         pragma Assert (Check_Queue_Consistency);
      end Signal;

   end Condition_PO;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is
   begin
      Condition_Index_Manager.Initialize;
      for J in The_Condition_PO_Arr'Range loop
         The_Condition_PO_Arr (J).Initialize (J);
      end loop;
      PTCV.Register_Condition_Factory (PTCV.Condition_Factory_Access
                                    (The_Condition_Factory));
   end Initialize;

   ------------
   -- Signal --
   ------------

   procedure Signal
     (C : in out Ravenscar_Condition_Type) is
      Someone_Is_Waiting : Boolean;
      To_Free            : Synchro_Index_Type;
   begin
      pragma Debug (O ("Signal"));
      The_Condition_PO_Arr (C.Id).Signal (Someone_Is_Waiting, To_Free);

      if Someone_Is_Waiting then
         Resume (To_Free);
      end if;
   end Signal;

   ----------
   -- Wait --
   ----------

   procedure Wait
     (C : in out Ravenscar_Condition_Type;
      M : access PTM.Mutex_Type'Class) is
      S : Synchro_Index_Type;
   begin
      pragma Debug (O ("Wait"));
      S := Prepare_Suspend;
      The_Condition_PO_Arr (C.Id).Prepare_Wait (S);
      PTM.Leave (M.all);
      Suspend (S);
      PTM.Enter (M.all);
   end Wait;

   use PolyORB.Initialization;
   use PolyORB.Initialization.String_Lists;
   use PolyORB.Utils.Strings;

begin
   Register_Module
     (Module_Info'
      (Name => +"tasking.profiles.ravenscar.condition_variables",
       Conflicts => Empty,
       Depends => Empty,
       Provides => +"tasking.condition_variables",
       Init => Initialize'Access));
end PolyORB.Tasking.Profiles.Ravenscar.Condition_Variables;
