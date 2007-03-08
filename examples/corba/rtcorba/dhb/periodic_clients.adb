------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                     P E R I O D I C _ C L I E N T S                      --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--           Copyright (C) 2006, Free Software Foundation, Inc.             --
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

with Ada.Dynamic_Priorities;
with Ada.Exceptions;
with Ada.Text_IO;

with CORBA.Object.Policies;
with CORBA.ORB;
with CORBA.Policy;

with RTCORBA.Current.Helper;
with RTCORBA.PriorityMapping;

with PolyORB.RTCORBA_P.Setup;

with PolyORB.Tasking.Condition_Variables;
with PolyORB.Tasking.Mutexes;
with PolyORB.Tasking.Threads;
with PolyORB.Utils.Report;

with Constants;
with DHB.Worker;
with Whetstone;

package body Periodic_Clients is

   use Ada.Text_IO;
   use PolyORB.Utils.Report;

   use DHB;

   use PolyORB.Tasking.Mutexes;
   use PolyORB.Tasking.Condition_Variables;

   Number_Of_Tasks : Natural := 0;
   Tasks_Initialized : Natural := 0;

   Initialization_Mutex   : Mutex_Access;
   Initialization_CV      : Condition_Access;

   Alarm_Mutex   : Mutex_Access;
   Alarm_CV      : Condition_Access;

   Missed_Deadline_Workload : DHB.KWIPS := DHB.KWIPS'Last;
   Missed_Deadline : Boolean := False;

   type Periodic_Runnable is new PolyORB.Tasking.Threads.Runnable with record
      Info : Periodic_Task_Information;
   end record;

   ---------
   -- Run --
   ---------

   procedure Run (R : access Periodic_Runnable);

   procedure Run (R : access Periodic_Runnable) is
      use Ada.Real_Time;
      use CORBA;
      use CORBA.ORB;
      use RTCORBA;

      Worker : DHB.Worker.Ref;

      Next_Top : Time;

      Server_Workload : DHB.KWIPS := R.Info.Initial_Server_Workload;

      Current : RTCORBA.Current.Local_Ref
        := RTCORBA.Current.Helper.To_Local_Ref
        (Resolve_Initial_References (To_CORBA_String ("RTCurrent")));

      Running_Priority : RTCORBA.Priority;
      Rounded_Priority : RTCORBA.Priority;
      Ok : Boolean := True;

      Inconsistent_Policies : CORBA.Policy.PolicyList;

   begin
      --  Getting the CORBA.Object

      CORBA.ORB.String_To_Object (R.Info.Worker_String_Ref, Worker);

      --  Checking if it worked

      if DHB.Worker.Is_Nil (Worker) then
         Output ("main : cannot invoke on a nil reference", False);
         return;
      end if;

      CORBA.Object.Policies.Validate_Connection
        (CORBA.Object.Ref (Worker), Inconsistent_Policies, Ok);

      if not Ok then
         Put_Line ("No connection possible, exiting.");
         return;
      end if;

      --  Set up client's RT-CORBA Priority

      RTCORBA.Current.Set_The_Priority (Current, R.Info.Client_Priority);

      if Constants.Verbose then
         Output ("Set RTCurrent priority to "
                 & RTCORBA.Priority'Image (R.Info.Client_Priority), True);
      end if;

      --  Computing rounded priority

      RTCORBA.PriorityMapping.To_CORBA
        (PolyORB.RTCORBA_P.Setup.Get_Priority_Mapping.all,
         RTCORBA.NativePriority (Ada.Dynamic_Priorities.Get_Priority),
         Rounded_Priority,
         Ok);

      if not Ok then
         raise Program_Error;
      end if;

      --  Check Worker's priority is correct

      Output ("Getting running priority", True);

      Running_Priority := DHB.Worker.Running_Priority (Worker);

      if Constants.Verbose then
         Output ("Running priority is correct",
                 Running_Priority = Rounded_Priority);

         Output ("Initialization completed, waiting", True);
      end if;

      Enter (Initialization_Mutex);
      Tasks_Initialized := Tasks_Initialized + 1;
      Wait (Initialization_CV, Initialization_Mutex);
      Leave (Initialization_Mutex);

      if Constants.Verbose then
         Output ("Begin test", True);
      end if;

      --  Do some work periodically, increasing server workload, until
      --  a deadline is missed.

      Next_Top := Ada.Real_Time.Clock;

      loop
         Whetstone.Small_Whetstone (R.Info.Client_Workload);

         DHB.Worker.Do_Some_Work (Worker, Server_Workload);

         Next_Top := Next_Top + R.Info.Period;

         exit when Missed_Deadline
           or else Next_Top - Clock <= To_Time_Span (0.0);

         Server_Workload := Server_Workload + R.Info.Server_Workload_Increment;

         delay until Next_Top;
      end loop;

      --  Print result

      if Constants.Verbose then
         Output ("Missed deadline by task" & Natural'Image (R.Info.Id)
                 & " for Server_Workload ="
                 & DHB.KWIPS'Image (Server_Workload) & " KWIPS", True);
      end if;

      Enter (Alarm_Mutex);
      Missed_Deadline_Workload
        := DHB.KWIPS'Min (Missed_Deadline_Workload, Server_Workload);
      Missed_Deadline := True;
      Signal (Alarm_CV);
      Leave (Alarm_Mutex);

   exception
      when E : others =>
         Output ("Got exception !", False);
         Ada.Text_IO.Put_Line (Ada.Exceptions.Exception_Information (E));
   end Run;

   ----------------
   -- Run_Test_1 --
   ----------------

   procedure Run_Test_1 (PTA : Periodic_Task_Array) is

      use PolyORB.Tasking.Threads;
      use Ada.Real_Time;

      Next_Top : Time := Clock;

   begin
      New_Test ("Periodic Test #1");
      Put_Line ("Description: Spawn N tasks, increase communication jitter "
                & "until one task misses its deadline");

      --  Initialize

      Create (Alarm_Mutex);
      Create (Alarm_CV);
      Create (Initialization_Mutex);
      Create (Initialization_CV);

      --  Spawn all tasks

      Enter (Alarm_Mutex);
      Number_Of_Tasks := PTA'Length;
      Leave (Alarm_Mutex);

      for J in PTA'Range loop
         declare
            New_Periodic_Task : constant Runnable_Access
              := new Periodic_Runnable;

         begin
            Periodic_Runnable (New_Periodic_Task.all).Info := PTA (J);

            declare
               T : constant Thread_Access :=
                 Run_In_Task
                 (TF               => Get_Thread_Factory,
                  Name             => "",
                  Default_Priority => 15,
                  Storage_Size     => 0,
                  R                => New_Periodic_Task,
                  C                => new Runnable_Controller);
               pragma Unreferenced (T);
            begin
               null;
            end;
         end;
      end loop;

      --  Wait for all tasks to be initialized

      while Tasks_Initialized < Number_Of_Tasks loop
         Next_Top := Clock + To_Time_Span (0.5);
         delay until Next_Top;
      end loop;

      Enter (Initialization_Mutex);
      Broadcast (Initialization_CV);
      Leave (Initialization_Mutex);

      --  Wait for all tasks completing

      Enter (Alarm_Mutex);
      Wait (Alarm_CV, Alarm_Mutex);
      Leave (Alarm_Mutex);

      Output ("Missing deadline for workload = "
              & KWIPS'Image (Missed_Deadline_Workload), True);
   end Run_Test_1;

   ----------------
   -- Run_Test_2 --
   ----------------

   procedure Run_Test_2 (Worker_String_Ref : CORBA.String) is
      use Ada.Real_Time;
      use CORBA;
      use CORBA.ORB;
      use RTCORBA;

      use DHB.Worker;

      Worker : DHB.Worker.Ref;

      Next_Top : Time;

      Float_Period : constant Duration := 0.1;
      Period : constant Time_Span := To_Time_Span (Float_Period);

      Seq : U_sequence
        := U_sequence (IDL_SEQUENCE_unsigned_long.Null_Sequence);

   begin
      New_Test ("Periodic Test #2");
      Put_Line ("Description: Invoke Do_Some_Work_With_Payload with an "
                & "increasing payload until this operation takes more than"
                & Duration'Image (Float_Period) & "s");

      --  Getting the CORBA.Object

      CORBA.ORB.String_To_Object (Worker_String_Ref, Worker);

      --  Checking if it worked

      if DHB.Worker.Is_Nil (Worker) then
         Output ("main : cannot invoke on a nil reference", False);
      end if;

      if Constants.Verbose then
         Output ("Begin test", True);
      end if;

      --  Do some work periodically, increasing server workload, until
      --  a deadline is missed.

      Next_Top := Ada.Real_Time.Clock;

      loop
         DHB.Worker.Do_Some_Work_With_Payload (Worker, DHB.KWIPS'(0), Seq);
         Next_Top := Next_Top + Period;
         exit when Next_Top < Clock;
         Seq := Seq & To_Sequence (1024);

         delay until Next_Top;
      end loop;

      Output ("BandWidth: "
              & Float'Image
              (Float (Length (Seq)) * Float (CORBA.Unsigned_Long'Size)
               / Float (Float_Period)),
              True);
   end Run_Test_2;

end Periodic_Clients;
