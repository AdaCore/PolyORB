------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--             P O L Y O R B . T A S K I N G . P R O F I L E S              --
--                   . R A V E N S C A R . M U T E X E S                    --
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

with PolyORB.Tasking.Threads;
with PolyORB.Tasking.Profiles.Ravenscar.Threads;

with PolyORB.Initialization;
with PolyORB.Utils.Strings;

package body PolyORB.Tasking.Profiles.Ravenscar.Mutexes is
   use PolyORB.Tasking.Profiles.Ravenscar.Threads;

   package PTT renames PolyORB.Tasking.Threads;

   package PTM renames PolyORB.Tasking.Mutexes;

   type Queued_Thread is record
      --  Element of a queue of Thread; see comment for Thread_Queue.

      This       : Ravenscar_Thread_Id;
      Next       : Extended_Thread_Index;
      Is_Waiting : Boolean;
   end record;

   type Thread_Queue is array (Thread_Index) of Queued_Thread;
   --  Implementation of a queue using an array.
   --  Each element of the array contain an access to a Thread,
   --  and the index of the Thread following it in the queue.
   --  This queue  is used by a Mutex to record the tasks that
   --  wait for it.
   --  The place of a Thread in the array doesn't change during execution;
   --  The only thing that changes is the index of the Thread that follows it
   --  in the queue. If it is not queued, it is set to Null_Thread_Index.

   type Mutex_Pool_Type is array (Mutex_Index_Type)
     of aliased Ravenscar_Mutex_Type;

   The_Mutex_Pool : Mutex_Pool_Type;
   --  The pool of preallocated mutexes.

   protected type Mutex_PO is
      --  Provide thread safe primitives for a  Mutex,
      --  and manage its Thread_Queue.

      procedure Test_And_Set_Entry
        (Result : out Boolean;
         Place  : Thread_Index;
         T      : Ravenscar_Thread_Id);
      --  Test if the task can enter the Mutex (i.e, if no other
      --  task own the monitor). If so, it take it; If it cannot,
      --  it is queued.

      procedure Leave
        (Someone_Is_Waiting : out Boolean;
         To_Free            : out Ravenscar_Thread_Id);
      --  Free the Mutex.

      procedure Initialize (N : Mutex_Index_Type);
      --  Initialize the Mutex.

   private
      My_Index         : Mutex_Index_Type;
      Owner            : Extended_Thread_Index;
      Waiters          : Thread_Queue;
   end Mutex_PO;

   type Mutex_PO_Arr is array (Mutex_Index_Manager.Index_Type)
     of Mutex_PO;

   The_Mutex_PO_Arr : Mutex_PO_Arr;
   --  Pool of Mutex_PO.

   ------------
   -- Create --
   ------------

   function Create
     (MF   : access Ravenscar_Mutex_Factory_Type;
      Name : String := "")
     return Mutex_Access is
      pragma Warnings (Off);
      pragma Unreferenced (MF);
      pragma Unreferenced (Name);
      pragma Warnings (On);
      --  XXX The use of names is not implemented yet.
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
     (MF : in out Ravenscar_Mutex_Factory_Type;
      M  : in out Mutex_Access) is
      pragma Warnings (Off);
      pragma Unreferenced (MF);
      pragma Warnings (On);
   begin
      Mutex_Index_Manager.Release (Ravenscar_Mutex_Access (M).Id);
   end Destroy;

   -----------
   -- Enter --
   -----------

   procedure Enter (M : in out Ravenscar_Mutex_Type) is
      Exit_Condition : Boolean;
      Place          : Thread_Index;
      My_TF          : constant PTT.Thread_Factory_Access
        := PTT.Get_Thread_Factory;
      Current_Thread_Id : constant Ravenscar_Thread_Id
        := Ravenscar_Thread_Id (PTT.Get_Current_Thread_Id (My_TF));
   begin
      Place := Get_Thread_Index (Current_Thread_Id);
      The_Mutex_PO_Arr (M.Id).Test_And_Set_Entry
        (Exit_Condition,
         Place,
         Current_Thread_Id);
      if not Exit_Condition then
         Determinist_Suspend (Current_Thread_Id);
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
      PTM.Register_Mutex_Factory (PTM.Mutex_Factory_Access
                                    (The_Mutex_Factory));
   end Initialize;

   -----------
   -- Leave --
   -----------

   procedure Leave (M : in out Ravenscar_Mutex_Type) is
      To_Free            : Ravenscar_Thread_Id;
      Someone_Is_Waiting : Boolean;
   begin
      The_Mutex_PO_Arr (M.Id).Leave (Someone_Is_Waiting, To_Free);

      if Someone_Is_Waiting then
         Determinist_Resume (To_Free);
      end if;
   end Leave;

   --------------
   -- Mutex_PO --
   --------------

   protected body Mutex_PO is

      -------------------------
      -- Mutex_PO.Initialize --
      -------------------------

      procedure Initialize (N : Mutex_Index_Type) is
      begin
         My_Index := N;
         Owner := Null_Thread_Index;
         for J in Waiters'Range loop
            Waiters (J).Next := Null_Thread_Index;
            Waiters (J).Is_Waiting  := False;
         end loop;
      end Initialize;

      --------------------
      -- Mutex_PO.Leave --
      --------------------

      procedure Leave
        (Someone_Is_Waiting : out Boolean;
         To_Free            : out Ravenscar_Thread_Id) is
         Former_Owner : constant Extended_Thread_Index := Owner;
      begin
         pragma Assert (Owner /= Null_Thread_Index);

         if Waiters (Former_Owner).Next /= Null_Thread_Index then
            Owner := Waiters (Former_Owner).Next;
            pragma Assert (Waiters (Owner).Is_Waiting = True);
            Someone_Is_Waiting := True;
            To_Free := Waiters (Owner).This;
         else
            Someone_Is_Waiting := False;
            Owner := Null_Thread_Index;
         end if;

         Waiters (Former_Owner).Is_Waiting := False;
         Waiters (Former_Owner).Next := Null_Thread_Index;

      end Leave;

      ---------------------------------
      -- Mutex_PO.Test_And_Set_Entry --
      ---------------------------------

      procedure Test_And_Set_Entry
        (Result : out Boolean;
         Place  : Thread_Index;
         T      : Ravenscar_Thread_Id) is
         Current   : Extended_Thread_Index;
         Precedent : Extended_Thread_Index;
      begin
         Result :=  Owner = Null_Thread_Index;
         pragma Assert (Owner = Null_Thread_Index
                        or else Waiters (Owner).This /= T);
         --  Fail if we try to enter the Mutex for the second time

         Waiters (Place).Is_Waiting := True;
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
            Waiters (Place).Is_Waiting := False;
         elsif Precedent = Null_Thread_Index then
            Waiters (Place).Is_Waiting := True;
            Waiters (Place).Next := Waiters (Owner).Next;
            Waiters (Owner).Next := Place;
         else
            Waiters (Place).Is_Waiting := True;
            Waiters (Precedent).Next := Place;
            Waiters (Place).Next := Current;
         end if;

      end Test_And_Set_Entry;

   end Mutex_PO;

   use PolyORB.Initialization;
   use PolyORB.Initialization.String_Lists;
   use PolyORB.Utils.Strings;

begin
   Register_Module
     (Module_Info'
      (Name => +"tasking.profiles.ravenscar.mutexes",
       Conflicts => Empty,
       Depends => Empty,
       Provides => +"tasking.mutexes",
       Init => Initialize'Access));
end PolyORB.Tasking.Profiles.Ravenscar.Mutexes;
