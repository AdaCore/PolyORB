------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                        P O L Y O R B . L A N E S                         --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--            Copyright (C) 2004 Free Software Foundation, Inc.             --
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

--  $Id$

with PolyORB.Log;
with PolyORB.ORB;
with PolyORB.Request_QoS;

package body PolyORB.Lanes is

   use PolyORB.Log;
   use PolyORB.Tasking.Condition_Variables;
   use PolyORB.Tasking.Mutexes;

   package L is new PolyORB.Log.Facility_Log ("polyorb.lanes");
   procedure O (Message : in String; Level : Log_Level := Debug)
     renames L.Output;

   procedure Idle (R : access Lane_Runnable);
   pragma Inline (Idle);
   --  Do anything required to put R in an idle state

   ---------
   -- Run --
   ---------

   procedure Run (R : access Lane_Runnable) is
   begin
      pragma Debug (O ("Entering lane's main loop"));

      Enter (R.L.Lock);
      loop
         pragma Debug (O ("Inside lane's main loop"));

         --  If R has a job to process, go for it

         if R.J /= null then
            pragma Debug (O ("Process locally queued job"));

            Leave (R.L.Lock);
            PolyORB.ORB.Run (PolyORB.ORB.Request_Job (R.J.all)'Access);
            Enter (R.L.Lock);

            R.J := null;
         end if;

         --  Then process queued jobs

         if R.L.Job_Queue /= null then
            while not Is_Empty (R.L.Job_Queue) loop
               pragma Debug (O ("Thread from lane at priority ("
                                & R.L.ORB_Priority'Img & ","
                                & R.L.Ext_Priority'Img & ")"
                                & " will execute a job"));
               declare
                  Job : constant Job_Access := Fetch_Job (R.L.Job_Queue);

               begin
                  Leave (R.L.Lock);
                  PolyORB.ORB.Run (PolyORB.ORB.Request_Job (Job.all)'Access);
                  Enter (R.L.Lock);
               end;
            end loop;
         end if;

         --  Test wether the task shall exit

         exit when R.L.Clean_Up_In_Progress or else R.Dynamically_Allocated;

         --  else go idle

         pragma Debug (O ("No job to process, go idle"));

         Idle (R);

      end loop;

      Leave (R.L.Lock);
      pragma Debug (O ("Exiting from lane's main loop"));
   end Run;

   --------------
   -- Add_Lane --
   --------------

   procedure Add_Lane
     (Set   : in out Lanes_Set;
      L     :        Lane_Access;
      Index :        Positive)
   is
   begin
      Set.Set (Index) := L;
   end Add_Lane;

   -------------------
   -- Attach_Thread --
   -------------------

   procedure Attach_Thread (EL : in out Extensible_Lane; T : Thread_Access) is
   begin
      --  XXX hummm, should be some consistency tests to ensure EL's
      --  priority is compatible with T priority

      Thread_Lists.Append (EL.Additional_Threads, T);
   end Attach_Thread;

   ------------
   -- Create --
   ------------

   function Create
     (ORB_Priority              : ORB_Component_Priority;
      Ext_Priority              : External_Priority;
      Base_Number_Of_Threads    : Natural;
      Dynamic_Number_Of_Threads : Natural;
      Stack_Size                : Natural;
      Buffer_Request            : Boolean;
      Max_Buffered_Requests     : PolyORB.Types.Unsigned_Long;
      Max_Buffer_Size           : PolyORB.Types.Unsigned_Long)
    return Lane_Access
   is
      Result : constant Lane_Access
        := new Lane (ORB_Priority => ORB_Priority,
                     Ext_Priority => Ext_Priority,
                     Base_Number_Of_Threads => Base_Number_Of_Threads,
                     Dynamic_Number_Of_Threads => Dynamic_Number_Of_Threads,
                     Stack_Size => Stack_Size,
                     Buffer_Request => Buffer_Request,
                     Max_Buffered_Requests => Max_Buffered_Requests,
                     Max_Buffer_Size => Max_Buffer_Size);

   begin
      pragma Debug (O ("Creating lane with"
                       & Positive'Image (Base_Number_Of_Threads)
                       & " threads at priority ("
                       & ORB_Priority'Img & ","
                       & Ext_Priority'Img & ")"));

      Create (Result.Lock);

      if Buffer_Request then
         Result.Job_Queue := Create_Queue;
      end if;

      for J in 1 .. Base_Number_Of_Threads loop
         declare
            New_Runnable : constant Lane_Runnable_Access := new Lane_Runnable;

         begin
            New_Runnable.L := Result;
            New_Runnable.Dynamically_Allocated := False;

            declare
               T : constant Thread_Access :=
                 Run_In_Task
                 (TF               => Get_Thread_Factory,
                  Name             => "",
                  Default_Priority => ORB_Priority,
                  Storage_Size     => Stack_Size,
                  R                => Runnable_Access (New_Runnable),
                  C                => new Runnable_Controller);
               pragma Unreferenced (T);
            begin
               null;
            end;
         end;
      end loop;

      return Result;
   end Create;

   -------------
   -- Destroy --
   -------------

   procedure Destroy (L : access Lane) is
      use type Idle_Task_Lists.List;

      Task_To_Awake : Idle_Task;

   begin
      L.Clean_Up_In_Progress := True;

      while L.Idle_Task_List /= Idle_Task_Lists.Empty loop
         pragma Debug (O ("Awake one idle task"));

         Idle_Task_Lists.Extract_First (L.Idle_Task_List, Task_To_Awake);
         Signal (Task_To_Awake.CV);
         Destroy (Task_To_Awake.CV);
      end loop;

   end Destroy;

   procedure Destroy (L : access Lanes_Set) is
   begin
      for J in L.Set'Range loop
         Destroy (L.Set (J));
      end loop;
   end Destroy;

   ---------------
   -- Queue_Job --
   ---------------

   procedure Queue_Job
     (L             : access Lane;
      J             :        Job_Access;
      Hint_Priority :        External_Priority := Invalid_Priority)
   is
      pragma Unreferenced (Hint_Priority);

   begin
      pragma Debug (O ("Queue job in lane at priority ("
                       & L.ORB_Priority'Img & ","
                       & L.Ext_Priority'Img & ")"));

      Enter (L.Lock);

      --  First, try to directly queue job on one task

      declare
         use type Idle_Task_Lists.List;

         Task_To_Awake : Idle_Task;

      begin
         if L.Idle_Task_List /= Idle_Task_Lists.Empty then
            pragma Debug (O ("Queue job on one idle task"));

            --  Signal one idle task, and puts its CV in Free_CV list

            Idle_Task_Lists.Extract_First (L.Idle_Task_List, Task_To_Awake);
            Signal (Task_To_Awake.CV);
            CV_Lists.Append (L.Free_CV, Task_To_Awake.CV);

            Task_To_Awake.R.J := J;

            Leave (L.Lock);

            return;
         end if;
      end;

      --  else, queue job in common job queue

      if L.Buffer_Request
        and then Length (L.Job_Queue) < Natural (L.Max_Buffered_Requests)
      then
         pragma Debug (O ("Queue job on job queue"));

         Queue_Job (L.Job_Queue, J);
         Leave (L.Lock);

         --  Eventually, create a new task

         if L.Dynamic_Threads_Created < L.Dynamic_Number_Of_Threads then
            declare
               New_Runnable : constant Lane_Runnable_Access
                 := new Lane_Runnable;

            begin
               New_Runnable.L := Lane_Access (L);
               New_Runnable.Dynamically_Allocated := True;

               declare
                  T : constant Thread_Access :=
                    Run_In_Task
                    (TF               => Get_Thread_Factory,
                     Name             => "",
                     Default_Priority => L.ORB_Priority,
                     Storage_Size     => L.Stack_Size,
                     R                => Runnable_Access (New_Runnable),
                     C                => new Runnable_Controller);
                  pragma Unreferenced (T);
               begin
                  null;
               end;
            end;

            L.Dynamic_Threads_Created := L.Dynamic_Threads_Created + 1;
         end if;

         return;
      end if;

      --  Cannot queue job

      Leave (L.Lock);
      pragma Debug (O ("Cannot queue job"));
      raise Program_Error;

   end Queue_Job;

   procedure Queue_Job
     (L             : access Lanes_Set;
      J             :        Job_Access;
      Hint_Priority :        External_Priority := Invalid_Priority)
   is
      use PolyORB.Request_QoS;
      use PolyORB.Request_QoS.QoS_Parameter_Lists;

      RJ : PolyORB.ORB.Request_Job renames PolyORB.ORB.Request_Job (J.all);

   begin
      --  XXX should find a way to do this in O (1)

      declare
         Parameter : constant QoS_Parameter
           := Extract_Parameter (Static_Priority, RJ.Request);

      begin
         if Parameter.Kind = Static_Priority then
            pragma Debug (O ("About to queue a job at priority"
                             & Parameter.OP'Img));

            for K in L.Set'Range loop
               pragma Debug (O ("Testing lane, priority"
                                & L.Set (K).all.ORB_Priority'Img));

               if L.Set (K).all.Ext_Priority = Parameter.EP then
                  Queue_Job (L.Set (K), J);
                  return;
               end if;
            end loop;

         elsif Hint_Priority /= Invalid_Priority then
            pragma Debug (O ("About to queue a job at priority"
                             & Hint_Priority'Img));

            for K in L.Set'Range loop
               pragma Debug (O ("Testing lane, priority"
                                & L.Set (K).all.ORB_Priority'Img));

               if L.Set (K).all.Ext_Priority = Hint_Priority then
                  Queue_Job (L.Set (K), J);
                  return;
               end if;
            end loop;
         end if;

         pragma Debug
              (O ("Cannot queue job, no lane matches request priority"));
         raise Program_Error;

      end;
   end Queue_Job;

   ----------
   -- Idle --
   ----------

   procedure Idle (R : access Lane_Runnable) is
      use type CV_Lists.List;

      CV : Condition_Access;

   begin
      --  Allocate a CV to wait on

      if R.L.Free_CV /= CV_Lists.Empty then
         --  Use an existing CV, from Free_CV list

         CV_Lists.Extract_First (R.L.Free_CV, CV);
      else
         --  else allocate a new one

         Create (CV);
      end if;

      --  Append R to the list of idle tasks

      Idle_Task_Lists.Append
        (R.L.Idle_Task_List,
         Idle_Task'(CV => CV,
                    R  => Lane_Runnable (R.all)'Access));

      --  Go idle

      pragma Debug (O ("Task is now idle"));
      PTCV.Wait (CV, R.L.Lock);
   end Idle;

end PolyORB.Lanes;
