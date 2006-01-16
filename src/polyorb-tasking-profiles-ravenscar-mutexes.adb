------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--               POLYORB.TASKING.PROFILES.RAVENSCAR.MUTEXES                 --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2002-2006, Free Software Foundation, Inc.          --
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

--  Implementation of synchronisation objects under the ravenscar profile

with PolyORB.Log;
with PolyORB.Utils.Strings;

package body PolyORB.Tasking.Profiles.Ravenscar.Mutexes is

   use PolyORB.Log;

   package L is new PolyORB.Log.Facility_Log
     ("polyorb.tasking.profiles.ravenscar.mutexes");
   procedure O (Message : String; Level : Log_Level := Debug)
     renames L.Output;
   function C (Level : Log_Level := Debug) return Boolean
     renames L.Enabled;
   pragma Unreferenced (C); --  For conditional pragma Debug

   package PTM renames PolyORB.Tasking.Mutexes;

   type Queued_Thread is record
      --  Element of a queue of Thread; see comment for Thread_Queue

      Sync       : Synchro_Index_Type;
      --  Synchro object the Thread is waiting on

      Next       : Extended_Synchro_Index;
      --  Next Thread in the queue

      Is_Waiting : Boolean;
      --  True if the thread is waiting

   end record;

   type Thread_Queue is array (Synchro_Index_Type) of Queued_Thread;
   --  Implementation of a queue using an array.
   --  Each element of the array represent a waiting thread, and
   --  contain an access to the synchro object on which it is waiting,
   --  and the index of the Thread following it in the queue.
   --  This queue is used by a Mutex to record the tasks that
   --  wait for it.
   --  The place of a Thread in the array change at every suspending
   --  call; It is determinated by the index of its current synchro
   --  object. Each element of the array contain an access to a
   --  Thread, and the index of the Thread following it in the queue.

   type Mutex_Pool_Type is array (Mutex_Index_Type)
     of aliased Ravenscar_Mutex_Type;

   The_Mutex_Pool : Mutex_Pool_Type;
   --  The pool of preallocated mutexes

   protected type Mutex_PO is
      --  Provide thread safe primitives for a  Mutex,
      --  and manage its Thread_Queue.

      function Check_Queue_Consistency return Boolean;
      --  Make some tests on the consistency of the queue.

      procedure Test_And_Set_Entry
        (Result : out Boolean;
         Place  : Synchro_Index_Type);
      --  Test if the task can enter the Mutex (i.e, if no other
      --  task own the monitor). If so, it take it; If it cannot,
      --  it is queued.

      procedure Leave
        (Someone_Is_Waiting : out Boolean;
         To_Free            : out Synchro_Index_Type);
      --  Free the Mutex

      procedure Initialize (N : Mutex_Index_Type);
      --  Initialize the Mutex

   private
      My_Index         : Mutex_Index_Type;
      --  Index of the Mutex in the pool

      Next             : Extended_Synchro_Index;
      --  Index of the next Thread to resume in Waiters

      Waiters          : Thread_Queue;
      --  Queue of the Threads waiting for the mutex

      Is_Taken         : Boolean := False;
      --  is True if someone owns the Mutex; False otherwise

   end Mutex_PO;

   type Mutex_PO_Arr is array (Mutex_Index_Manager.Index_Type)
     of Mutex_PO;

   The_Mutex_PO_Arr : Mutex_PO_Arr;
   --  Pool of Mutex_PO

   ------------
   -- Create --
   ------------

   function Create
     (MF   : access Ravenscar_Mutex_Factory_Type;
      Name : String := "")
     return Mutex_Access
   is
      pragma Warnings (Off);
      pragma Unreferenced (MF);
      pragma Unreferenced (Name);
      pragma Warnings (On);
      --  XXX The use of names is not implemented yet

      Index : Mutex_Index_Type;
      M     : Ravenscar_Mutex_Access;
   begin
      Mutex_Index_Manager.Get (Index);
      M := The_Mutex_Pool (Index)'Access;
      M.Id := Index;
      The_Mutex_PO_Arr (M.Id).Initialize (M.Id);
      return Mutex_Access (M);
   end Create;

   -------------
   -- Destroy --
   -------------

   procedure Destroy
     (MF : access Ravenscar_Mutex_Factory_Type;
      M  : in out Mutex_Access)
   is
      pragma Warnings (Off);
      pragma Unreferenced (MF);
      pragma Warnings (On);

   begin
      Mutex_Index_Manager.Release (Ravenscar_Mutex_Access (M).Id);
   end Destroy;

   -----------
   -- Enter --
   -----------

   procedure Enter (M : access Ravenscar_Mutex_Type) is
      Exit_Condition : Boolean;
      S              : Synchro_Index_Type;

   begin
      pragma Debug (O ("Enter"));
      S := Prepare_Suspend;
      The_Mutex_PO_Arr (M.Id).Test_And_Set_Entry
        (Exit_Condition,
         S);
      if not Exit_Condition then
         Suspend (S);
      else
         Abort_Suspend (S);
      end if;
   end Enter;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is
   begin
      Mutex_Index_Manager.Initialize;
      for J in The_Mutex_PO_Arr'Range loop
         The_Mutex_PO_Arr (J).Initialize (J);
      end loop;
      PTM.Register_Mutex_Factory
        (PTM.Mutex_Factory_Access (The_Mutex_Factory));
   end Initialize;

   -----------
   -- Leave --
   -----------

   procedure Leave (M : access Ravenscar_Mutex_Type) is
      To_Free            : Synchro_Index_Type;
      Someone_Is_Waiting : Boolean;
   begin
      pragma Debug (O ("Leave"));
      The_Mutex_PO_Arr (M.Id).Leave (Someone_Is_Waiting, To_Free);

      if Someone_Is_Waiting then
         Resume (To_Free);
      end if;
   end Leave;

   --------------
   -- Mutex_PO --
   --------------

   protected body Mutex_PO is

      ------------------------------
      --  Check_Queue_Consistency --
      ------------------------------

      function Check_Queue_Consistency return Boolean is
         type Bool_Arr is array (Waiters'Range) of Boolean;
         Marked  : Bool_Arr;
         Current : Extended_Synchro_Index := Next;

      begin
         for J in Marked'Range loop
            Marked (J) := False;
         end loop;
         while Current /= Null_Synchro_Index loop

            if Marked (Synchro_Index_Type (Current)) then
               --  Loop in the queue
               pragma Debug (O ("loop in the queue!!!"));
               return False;
            end if;

            if not Waiters (Synchro_Index_Type (Current)).Is_Waiting then
               --  Someone is in the queue, but does not wait
               pragma Debug (O ("active task in the queue!!! Id= "
                                & Integer'Image (Current)));
               return False;
            end if;

            Marked (Synchro_Index_Type (Current)) := True;
            Current := Waiters (Synchro_Index_Type (Current)).Next;
         end loop;

         return Is_Taken or else Next = Null_Synchro_Index;
         --  The queue is not empty only if the mutex is not taken

      end Check_Queue_Consistency;

      -------------------------
      -- Mutex_PO.Initialize --
      -------------------------

      procedure Initialize (N : Mutex_Index_Type) is
      begin
         My_Index := N;
         Next := Null_Synchro_Index;
         Is_Taken := False;
         for J in Waiters'Range loop
            Waiters (J).Next := Null_Synchro_Index;
            Waiters (J).Is_Waiting  := False;
         end loop;
         pragma Assert (Check_Queue_Consistency);
      end Initialize;

      --------------------
      -- Mutex_PO.Leave --
      --------------------

      procedure Leave
        (Someone_Is_Waiting : out Boolean;
         To_Free            : out Synchro_Index_Type)
      is
         Former_Next : constant Extended_Synchro_Index := Next;

      begin
         pragma Assert (Check_Queue_Consistency);

         if Former_Next /= Null_Synchro_Index then
            Next := Waiters (Synchro_Index_Type (Former_Next)).Next;
            pragma Assert
              (Waiters (Synchro_Index_Type (Former_Next)).Is_Waiting);
            Is_Taken := True;
            To_Free := Waiters (Synchro_Index_Type (Former_Next)).Sync;
            Waiters (Synchro_Index_Type (Former_Next)).Is_Waiting := False;
            Waiters (Synchro_Index_Type (Former_Next)).Next
              := Null_Synchro_Index;
         else
            Is_Taken := False;
            Next := Null_Synchro_Index;
         end if;

         Someone_Is_Waiting := Is_Taken;
         pragma Assert (Check_Queue_Consistency);
      end Leave;

      ---------------------------------
      -- Mutex_PO.Test_And_Set_Entry --
      ---------------------------------

      procedure Test_And_Set_Entry
        (Result : out Boolean;
         Place  : Synchro_Index_Type)
      is
         Current   : Extended_Synchro_Index;
         Precedent : Extended_Synchro_Index;

      begin
         pragma Assert (Check_Queue_Consistency);
         Result := not Is_Taken;

         Waiters (Place).Is_Waiting := True;
         Waiters (Place).Sync := Place;

         if not Result then
            --  Search the rank of T in the queue:

            Current := Next;
            Precedent := Null_Synchro_Index;

            while Current /= Null_Synchro_Index loop
               --  XXX compare the Priorities
               Precedent := Current;
               Current := Waiters (Synchro_Index_Type (Current)).Next;
            end loop;
         end if;

         --  Insert T in the queue

         if Result then
            Is_Taken := True;
            pragma Assert (Next = Null_Synchro_Index);
            --  XXX useless
            --  If this assertion fails, it means that the mutex
            --  is not taken BUT the queue is not empty!

         elsif Precedent = Null_Synchro_Index then
            Waiters (Place).Is_Waiting := True;
            Waiters (Place).Next := Next;
            Next := Extended_Synchro_Index (Place);

         else
            Waiters (Place).Is_Waiting := True;
            Waiters (Synchro_Index_Type (Precedent)).Next
              := Extended_Synchro_Index (Place);
            Waiters (Place).Next := Current;
         end if;

         pragma Assert (Check_Queue_Consistency);
      end Test_And_Set_Entry;

   end Mutex_PO;

   use PolyORB.Initialization;
   use PolyORB.Initialization.String_Lists;
   use PolyORB.Utils.Strings;

begin
   Register_Module
     (Module_Info'
      (Name      => +"tasking.profiles.ravenscar.mutexes",
       Conflicts => Empty,
       Depends   => Empty,
       Provides  => +"tasking.mutexes",
       Implicit  => False,
       Init      => Initializer));
end PolyORB.Tasking.Profiles.Ravenscar.Mutexes;
