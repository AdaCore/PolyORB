------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--           P O L Y O R B . R A V E N S C A R . M O N I T O R S            --
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

--  Implementation of Monitors under the ravenscar profile.

--  $Id$

with PolyORB.Tasking.Threads;
with PolyORB.Ravenscar.Configuration;
with PolyORB.Ravenscar.Threads;

with PolyORB.Initialization;
with PolyORB.Utils.Strings;

package body PolyORB.Ravenscar.Monitors is
   use PolyORB.Ravenscar.Threads;

   package PTT renames PolyORB.Tasking.Threads;

   package PTM renames PolyORB.Tasking.Monitors;

   type Waiting_Type is (Enter_Wait, Condition_Wait, None);
   --  Type of event a task can be waiting :
   --  * Enter_Wait : the task wait for the monitor to be freed;
   --  * Condition_Wait : the task wait for a Condition to be fulfilled;
   --  * None : the task is not supposed to wait.

   type Queued_Thread is record
      --  Element of a queue of Thread; see comment for Thread_Queue.

      This : Ravenscar_Thread_Id;
      Next : Extended_Thread_Index;
      WT   : Waiting_Type;
   end record;

   type Thread_Queue is array (Thread_Index) of Queued_Thread;
   --  Implementation of a queue using an array.
   --  Each element of the array contain an access to a Thread,
   --  and the index of the Thread following it in the queue.
   --  This queue  is used by a Monitor to record the tasks that
   --  wait for it.
   --  The place of a Thread in the array doesn't change during execution;
   --  The only thing that changes is the index of the Thread that follows it
   --  in the queue. If it is not queued, it is set to Null_Thread_Index.

   type Monitor_Pool_Type is array (Monitor_Index_Type)
     of aliased Ravenscar_Monitor_Type;

   The_Monitor_Pool : Monitor_Pool_Type;
   --  The pool of preallocated monitors.

   protected type Protected_Infos is
      --  Provide thread safe primitives for a  Monitor,
      --  and manage its Thread_Queue.

      procedure Test_And_Set_Entry
        (Result : out Boolean;
         Place  : Thread_Index;
         T      : Ravenscar_Thread_Id);
      --  Test if the task can enter the Monitor (i.e, if no other
      --  task own the monitor). If so, it take it; If it cannot,
      --  it is queued.

      procedure Retest_Entry
        (Result : out Boolean;
         Place  : Thread_Index);
      --  Retest if the task can enter the Monitor, assuming that
      --  the task has already been queued.

      procedure Evaluate
        (Id     : Thread_Index;
         C      : access PTM.Condition_Type'Class;
         Result : out Boolean);
      --  Evaluate the condition in a critical section.

      procedure Leave_Entry;
      --  Free the Monitor.

      procedure Signal;
      --  Resume the waiting tasks, so that they can reevaluate their
      --  condition.

      procedure Initialize (N : Monitor_Index_Type);
      --  Initialize the Monitor.

      function Get_Owner return Extended_Thread_Index;
      --  return the Id of the owner of the Monitor.

   private
      My_Index         : Monitor_Index_Type;
      Owner            : Extended_Thread_Index;
      Waiters          : Thread_Queue;
   end Protected_Infos;

   type Protected_Infos_Arr is array (My_Index_Manager.Index_Type)
     of Protected_Infos;

   The_Infos_Arr : Protected_Infos_Arr;
   --  Pool of Protected_Infos.

   ------------
   -- Create --
   ------------

   function Create
     (MF   : access Ravenscar_Monitor_Factory_Type;
      Name : String := "")
     return Monitor_Access is
      pragma Warnings (Off);
      pragma Unreferenced (MF);
      pragma Unreferenced (Name);
      pragma Warnings (On);
      --  XXX The use of names is not implemented yet.
      Index : Monitor_Index_Type;
      M     : Ravenscar_Monitor_Access;
   begin
      My_Index_Manager.Get (Index);
      M := The_Monitor_Pool (Index)'Access;
      M.Id := Index;
      The_Infos_Arr (M.Id).Initialize (M.Id);
      return Monitor_Access (M);
   end Create;

   -------------
   -- Destroy --
   -------------

   procedure Destroy
     (MF : in out Ravenscar_Monitor_Factory_Type;
      M  : in out Monitor_Access) is
      pragma Warnings (Off);
      pragma Unreferenced (MF);
      pragma Warnings (On);
   begin
      My_Index_Manager.Release (Ravenscar_Monitor_Access (M).Id);
   end Destroy;

   -----------
   -- Enter --
   -----------

   procedure Enter (M : in out Ravenscar_Monitor_Type) is
      Exit_Condition : Boolean;
      Place          : Thread_Index;
      My_TF          : constant PTT.Thread_Factory_Access
        := PTT.Get_Thread_Factory;
      Current_Thread_Id : constant Ravenscar_Thread_Id
        := Ravenscar_Thread_Id (PTT.Get_Current_Thread_Id (My_TF));
   begin
      Place := Get_Thread_Index (Current_Thread_Id);
      The_Infos_Arr (M.Id).Test_And_Set_Entry
        (Exit_Condition,
         Place,
         Current_Thread_Id);
      while not Exit_Condition loop
         --  We need this loop, because we are not sure that
         --  the owner will call "Resume" first.
         --  pragma Assert (Current_Thread /= null);
         Suspend (Current_Thread_Id);
         The_Infos_Arr (M.Id).Retest_Entry
           (Exit_Condition,
            Place);
      end loop;
   end Enter;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is
   begin
      My_Index_Manager.Initialize;
      for J in The_Infos_Arr'Range loop
         The_Infos_Arr (J).Initialize (J);
      end loop;
      PTM.Register_Monitor_Factory (PTM.Monitor_Factory_Access
                                    (The_Monitor_Factory));
   end Initialize;

   -----------
   -- Leave --
   -----------

   procedure Leave (M : in out Ravenscar_Monitor_Type) is
   begin
      The_Infos_Arr (M.Id).Leave_Entry;
   end Leave;

   ---------------------
   -- Protected_Infos --
   ---------------------

   protected body Protected_Infos is

      ------------------------------
      -- Protected_Infos.Evaluate --
      ------------------------------

      procedure Evaluate
        (Id     : Thread_Index;
         C      : access PTM.Condition_Type'Class;
         Result : out Boolean) is
      begin
         PTM.Evaluate (C.all,
                       Result);
         if not Result then
            Waiters (Id).WT := Condition_Wait;
         end if;
      end Evaluate;

      -------------------------------
      -- Protected_Infos.Get_Owner --
      -------------------------------

      function Get_Owner return Extended_Thread_Index is
      begin
         return Owner;
      end Get_Owner;

      --------------------------------
      -- Protected_Infos.Initialize --
      --------------------------------

      procedure Initialize (N : Monitor_Index_Type) is
      begin
         My_Index := N;
         Owner := Null_Thread_Index;
         for J in Waiters'Range loop
            --  Waiters (J).This := null;
            Waiters (J).Next := Null_Thread_Index;
            Waiters (J).WT   := None;
         end loop;
      end Initialize;

      ---------------------------------
      -- Protected_Infos.Leave_Entry --
      ---------------------------------

      procedure Leave_Entry is
         Former_Owner : constant Extended_Thread_Index := Owner;
      begin
         pragma Assert (Owner /= Null_Thread_Index);

         if Waiters (Former_Owner).Next /= Null_Thread_Index then
            Owner := Waiters (Former_Owner).Next;
            --  pragma Assert (Waiters (Owner).This /= null);
            Resume (Waiters (Owner).This);
            Waiters (Owner).WT := None;
         else
            Owner := Null_Thread_Index;
         end if;

         if Waiters (Former_Owner).WT /= Condition_Wait then
            Waiters (Former_Owner).WT := None;
         end if;
      exception
         when others =>
            raise;
      end Leave_Entry;

      ----------------------------------
      -- Protected_Infos.Retest_Entry --
      ----------------------------------

      procedure Retest_Entry
        (Result : out Boolean;
         Place  : Thread_Index) is
      begin
         Result := Place = Owner
           or else Owner = Null_Thread_Index;
      end Retest_Entry;

      ----------------------------
      -- Protected_Infos.Signal --
      ----------------------------

      procedure Signal is
      begin
         for J in Waiters'Range loop
            if Waiters (J).WT = Condition_Wait then
               --  pragma Assert (Waiters (J).This /= null);
               Resume (Waiters (J).This);
            end if;
         end loop;
      end Signal;

      ----------------------------------------
      -- Protected_Infos.Test_And_Set_Entry --
      ----------------------------------------

      procedure Test_And_Set_Entry
        (Result : out Boolean;
         Place  : Thread_Index;
         T      : Ravenscar_Thread_Id) is
         Current   : Extended_Thread_Index;
         Precedent : Extended_Thread_Index;
      begin
         Result :=  Owner = Null_Thread_Index;
         --  pragma Assert (T /= null);
         pragma Assert (Owner = Null_Thread_Index
                        or else Waiters (Owner).This /= T);
         Waiters (Place).WT := Enter_Wait;
         Waiters (Place).This := T;

         if not Result then
            --  This loop search the rank of T in the queue:
            Current := Waiters (Owner).Next;
            Precedent := Null_Thread_Index;
            while Current /= Null_Thread_Index loop
               --  XXX compare the Priorities...
               Precedent := Current;
               Current := Waiters (Current).Next;
            end loop;
         end if;

         --  Now we insert T in the queue:
         if Result then
            Owner := Place;
            Waiters (Place).Next := Null_Thread_Index;
            Waiters (Place).WT := None;
         elsif Precedent = Null_Thread_Index then
            Waiters (Place).Next := Waiters (Owner).Next;
            Waiters (Owner).Next := Place;
         else
            Waiters (Precedent).Next := Place;
            Waiters (Place).Next := Current;
         end if;

      end Test_And_Set_Entry;

   end Protected_Infos;

   ------------
   -- Signal --
   ------------

   procedure Signal
     (M : in out Ravenscar_Monitor_Type) is
   begin
      The_Infos_Arr (M.Id).Signal;
   end Signal;

   ----------
   -- Wait --
   ----------

   procedure Wait
     (M : in out Ravenscar_Monitor_Type;
      C : access PTM.Condition_Type'Class) is
      Exit_Condition : Boolean;
      My_TF          : constant PTT.Thread_Factory_Access
        := PTT.Get_Thread_Factory;
      Current_Thread_Id : constant Ravenscar_Thread_Id
        := Ravenscar_Thread_Id (PTT.Get_Current_Thread_Id (My_TF));
      T_Id           : constant Extended_Thread_Index
        := Get_Thread_Index (Current_Thread_Id);
   begin

      if The_Infos_Arr (M.Id).Get_Owner /= T_Id then
         raise Program_Error;
      end if;

      The_Infos_Arr (M.Id).Evaluate
        (T_Id,
         C,
         Exit_Condition);

      while not Exit_Condition loop
         Leave (M);
         --  pragma Assert (Current_Thread /= null);
         Suspend (Current_Thread_Id);
         Enter (M);
         The_Infos_Arr (M.Id).Evaluate
           (Get_Thread_Index (Current_Thread_Id),
            C,
            Exit_Condition);

      end loop;
   end Wait;

   use PolyORB.Initialization;
   use PolyORB.Initialization.String_Lists;
   use PolyORB.Utils.Strings;

begin
   Register_Module
     (Module_Info'
      (Name => +"ravenscar-monitors",
       Conflicts => Empty,
       Depends => Empty,
       Provides => +"tasking-monitors",
       Init => Initialize'Access));
end PolyORB.Ravenscar.Monitors;
