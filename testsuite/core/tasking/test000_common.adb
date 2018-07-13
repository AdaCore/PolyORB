------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                       T E S T 0 0 0 _ C O M M O N                        --
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

pragma Ada_2012;

with Ada.Real_Time;
with Ada.Exceptions;

with PolyORB.Tasking.Threads;
with PolyORB.Tasking.Advanced_Mutexes;
with PolyORB.Tasking.Mutexes;
with PolyORB.Tasking.Condition_Variables;

with PolyORB.Log;
with PolyORB.Utils.Report;

package body Test000_Common is

   use Ada.Exceptions;

   use PolyORB.Log;
   use PolyORB.Tasking.Threads;

   package PTMX renames PolyORB.Tasking.Advanced_Mutexes;
   package PTCV renames PolyORB.Tasking.Condition_Variables;
   package PTM  renames PolyORB.Tasking.Mutexes;
   package PTT  renames PolyORB.Tasking.Threads;

   package L is new PolyORB.Log.Facility_Log ("polyorb.tasking.test");
   procedure O (Message : String; Level : Log_Level := Debug)
     renames L.Output;
   function C (Level : Log_Level := Debug) return Boolean
     renames L.Enabled;
   pragma Unreferenced (C); --  For conditional pragma Debug

   procedure Tempo (Time_In_Seconds : Float := Delay_Used);
   --  Wait for Time_In_Seconds seconds.

   ----------------------------------------------
   -- Types and variables used in Thread tests --
   ----------------------------------------------

   My_Thread_Factory  : PTT.Thread_Factory_Access;

   procedure Test_1;
   --  Main procedure of the tasks of the first Thread test.
   --  Test that the good number of tasks are created.

   procedure Test_2;
   --  Main procedure of the tasks of the second Thread test.
   --  Test Get_Current_Thread.

   ---------------------------------------------
   -- Types and variables used in Mutex tests --
   ---------------------------------------------

   My_Mutex   : PTMX.Adv_Mutex_Access;

   procedure Task_Test_Mutexes (Id : Integer);
   --  Main procedure for the tasks of the Mutex tests.

   ---------------------------------------------------
   -- Types and Variables for Test_Synchronisations --
   ---------------------------------------------------

   My_SCondition         : PTCV.Condition_Access;

   My_SMutex             : PTM.Mutex_Access;

   My_SBoolean           : Boolean;
   --  Boolean used for the tests, in association with My_SCondition

   procedure Task_Test_Synchronisations (Id : Integer);
   --  Main procedure for the tasks of the synchro tests.
   --  It is adapted from the the monitor test.

   Use_Broadcast : constant Boolean := True;

   procedure Signal;
   --  Signal all the task waiting on M_SCondition.
   --  Two implementations are provided for it:
   --  * one uses Broadcast (My_SCondition);
   --  * one uses Signal (My_SCondition).

   -------------------------------------------
   -- Types and variables used in all tests --
   -------------------------------------------

   subtype Task_Index is Integer range 1 .. Number_Of_Tasks;

   type Generic_Run is new PTT.Runnable with record
      Id : Task_Index;
      P  : PTT.Parameterless_Procedure;
   end record;
   --  Simple generic Runnable, that use a access to procedure
   --  for its main procedure

   overriding procedure Run (R : not null access Generic_Run);
   --  Call to R.P.all

   Ok             : Boolean := True;
   pragma Volatile (Ok);

   J              : Integer := 0;
   pragma Volatile (J);

   --  (On the following code, packages with a private Mutex are used to
   --  emulate protected object. The reason is is build this way
   --  is that it is based on tests that used protected objects,
   --  so we wanted to minimalize the changes).

   package Synchro is
      --  Protected object used to synchronize the tasks of the test.
      --  We assume that test number go increasingly, from 0.
      --  Tests can wait on a test number; if the internal number of
      --  Synchro is superior, the waiting test continue is execution.

      procedure Initialize;

      procedure Wait (Version : Integer);
      --  Await until Synchro.Version >= Synchro

      procedure Signal (Version : Integer);
      --  Change the Version number. If Version < Synchro.Version,
      --  a assertion failure occurs.

      procedure Reset;
      --  Set the Version number to 0.

      --  The followings are common barrier facilities :

      procedure Simple_Wait;
      --  Simple wait (barrier type).

      procedure Reset_Simple_Wait;
      --  Reset the barrier

      procedure Simple_Release;
      --  Signal the barrier.

   private
      Passing               : Boolean := True;
      Signaled              : Boolean := False;
      My_Version            : Integer := 0;
      Await_Count           : Integer := 0;
      Internal_Condition    : PTCV.Condition_Access;
      Internal_Mutex        : PTM.Mutex_Access;
   end Synchro;

   package Synchro_Joiner is
      --  Protected object used to synchronize the test driver with the end
      --  of a  test.
      --  When all the tasks of the test have called Signal_End,
      --  the tasks blocking on Join are freed.

      procedure Initialize;

      procedure Join;

      procedure Signal_End;

   private
      Passing            : Boolean := False;
      Number_Ended       : Natural := 0;
      Internal_Mutex     : PTM.Mutex_Access;
      Internal_Condition : PTCV.Condition_Access;
   end Synchro_Joiner;

   package Counter is
      --  Synchronized counter.

      procedure Increase;
      procedure Initialize;
      function Get return Integer;
      procedure Reset;
   private
      Internal_Mutex : PTM.Mutex_Access;
      Count            : Natural := 0;
      pragma Atomic (Count);
   end Counter;

   type Runnable_Arr is array (Task_Index) of Runnable_Access;
   R  : Runnable_Arr;

   type Identified_Runnable_Main_Procedure is
     access procedure (Id : Integer);

   type Identified_Runnable is new PTT.Runnable with record
      Id : Integer;
      P  : Identified_Runnable_Main_Procedure;
   end record;

   overriding procedure Run (R : not null access Identified_Runnable);

   type Id_Runnable_Arr is array (Task_Index) of Runnable_Access;
   Id_R  : Id_Runnable_Arr;

   -------------
   -- Counter --
   -------------

   package body Counter is

      -----------------
      -- Counter.Get --
      -----------------

      function Get return Integer is
      begin
         return Count;
      end Get;

      ----------------------
      -- Counter.Increase --
      ----------------------

      procedure Increase is
      begin
         PTM.Enter (Internal_Mutex);
         Count := Count + 1;
         PTM.Leave (Internal_Mutex);
      end Increase;

      ------------------------
      -- Counter.Initialize --
      ------------------------

      procedure Initialize is
      begin
         PTM.Create (Internal_Mutex);
      end Initialize;

      -------------------
      -- Counter.Reset --
      -------------------

      procedure Reset is
      begin
         PTM.Enter (Internal_Mutex);
         Count := 0;
         PTM.Leave (Internal_Mutex);
      end Reset;

   end Counter;

   ---------------
   -- End_Tests --
   ---------------

   procedure End_Tests is
   begin
      PolyORB.Utils.Report.Output ("END TESTS", True);
   end End_Tests;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is

   begin
      My_Thread_Factory := PTT.Get_Thread_Factory;
      PTM.Create (My_SMutex);
      PTCV.Create (My_SCondition);
      Counter.Initialize;
      Synchro_Joiner.Initialize;
      Synchro.Initialize;
   end Initialize;

   ---------
   -- Run --
   ---------

   overriding procedure Run (R : not null access Generic_Run) is
   begin
      R.P.all;
   end Run;

   overriding procedure Run (R : not null access Identified_Runnable) is
   begin
      R.P.all (R.Id);
   end Run;

   ------------
   -- Signal --
   ------------

   procedure Signal is
   begin
      if Use_Broadcast then
         PTCV.Broadcast (My_SCondition);
      else
         for J in Task_Index'Range loop
            PTCV.Signal (My_SCondition);
         end loop;
      end if;
   end Signal;

   -------------
   -- Synchro --
   -------------

   package body Synchro is

      ------------------------
      -- Synchro.Initialize --
      ------------------------

      procedure Initialize is
      begin
         PTM.Create (Internal_Mutex);
         PTCV.Create (Internal_Condition);
      end Initialize;

      -------------------
      -- Synchro.Reset --
      -------------------

      procedure Reset is
      begin
         PTM.Enter (Internal_Mutex);
         My_Version := 0;
         Passing := True;
         Signaled := False;
         PTM.Leave (Internal_Mutex);
      end Reset;

      -------------------------------
      -- Synchro.Reset_Simple_Wait --
      -------------------------------

      procedure Reset_Simple_Wait is
      begin
         PTM.Enter (Internal_Mutex);
         Signaled := False;
         PTCV.Broadcast (Internal_Condition);
         PTM.Leave (Internal_Mutex);
      end Reset_Simple_Wait;

      --------------------
      -- Synchro.Signal --
      --------------------

      procedure Signal (Version : Integer) is
      begin
         PTM.Enter (Internal_Mutex);
         pragma Assert (Version > My_Version);
         My_Version := Version;
         if Await_Count /= 0 then
            Passing := False;
            PTCV.Broadcast (Internal_Condition);
         end if;
         PTM.Leave (Internal_Mutex);
      end Signal;

      ----------------------------
      -- Synchro.Simple_Release --
      ----------------------------

      procedure Simple_Release is
      begin
         PTM.Enter (Internal_Mutex);
         Signaled := True;
         PTCV.Broadcast (Internal_Condition);
         PTM.Leave (Internal_Mutex);
      end Simple_Release;

      -------------------------
      -- Synchro.Simple_Wait --
      -------------------------

      procedure Simple_Wait is
      begin
         PTM.Enter (Internal_Mutex);
         while not Signaled loop
            PTCV.Wait (Internal_Condition, Internal_Mutex);
         end loop;
         PTM.Leave (Internal_Mutex);
      end Simple_Wait;

      ------------------
      -- Synchro.Wait --
      ------------------

      procedure Wait (Version : Integer) is
      begin
         PTM.Enter (Internal_Mutex);
         while My_Version < Version loop
            --  Real_Wait :
            while not Passing loop
               PTCV.Wait (Internal_Condition, Internal_Mutex);
            end loop;
            if My_Version < Version then
               Await_Count := Await_Count + 1;
               while Passing loop
                  PTCV.Wait (Internal_Condition, Internal_Mutex);
               end loop;
               Await_Count := Await_Count - 1;
               if Await_Count = 0 then
                  Passing := True;
                  PTCV.Broadcast (Internal_Condition);
               end if;
            end if;
         end loop;
         PTM.Leave (Internal_Mutex);
      end Wait;
   end Synchro;

   --------------------
   -- Synchro_Joiner --
   --------------------

   package body Synchro_Joiner is

      -------------------------------
      -- Synchro_Joiner.Initialize --
      -------------------------------

      procedure Initialize is
      begin
         PTM.Create (Internal_Mutex);
         PTCV.Create (Internal_Condition);
      end Initialize;

      -------------------------
      -- Synchro_Joiner.Join --
      -------------------------

      procedure Join is
      begin
         PTM.Enter (Internal_Mutex);
         while not Passing loop
            PTCV.Wait (Internal_Condition, Internal_Mutex);
         end loop;
         Number_Ended := Number_Ended - Number_Of_Tasks;
         Passing := False;
         PTM.Leave (Internal_Mutex);
      end Join;

      -------------------------------
      -- Synchro_Joiner.Signal_End --
      -------------------------------

      procedure Signal_End is
      begin
         PTM.Enter (Internal_Mutex);
         Number_Ended := Number_Ended + 1;
         Passing := Number_Ended >= Number_Of_Tasks;
         PTCV.Broadcast (Internal_Condition);
         PTM.Leave (Internal_Mutex);
      end Signal_End;
   end Synchro_Joiner;

   --------------------------------
   -- Task_Test_Synchronisations --
   --------------------------------

   procedure Task_Test_Synchronisations (Id : Integer) is
      use PolyORB.Tasking.Mutexes;
      use PolyORB.Tasking.Condition_Variables;
   begin
      O ("task "
         & Integer'Image (Id)
         & " begins Synchronisation tests ");
      Synchro.Simple_Wait;
      Synchro.Wait (1);
      --  Test the mutual exclusion.
      --  Every tasks try to take the mutex to change J.
      --  If two tasks take it at the same time, one will
      --  see J with a different value that the one she
      --  assigned.
      O ("task "
         & Integer'Image (Id)
         & " begins test on mutual exclusion ");
      begin
         Enter (My_SMutex);
         O ("task "
            & Integer'Image (Id)
            & " is in the mutex");
         J := Id;
         Tempo;
         Ok := Ok and then (J = Id);
         O ("task "
            & Integer'Image (Id)
            & " will leave in the mutex");
         Leave (My_SMutex);
      exception
         when Exc : others =>
            Ok := False;
            O ("task "
               & Integer'Image (Id)
               & " EXCEPTION RAISED ! "
               & Exception_Name (Exc)
               & " : "
               & Exception_Message (Exc));
      end;
      Synchro_Joiner.Signal_End;

      Synchro.Wait (2);
      --  Test that a call to Wait free the mutex
      --  Every task change J, then Wait on My_Condition.
      --  When all of them are waiting, they are released.
      O ("task "
         & Integer'Image (Id)
         & " tests that a  call to Wait free the mutex");
      begin
         Enter (My_SMutex);
         O ("task "
            & Integer'Image (Id)
            & " is in the mutex");
         J := Id;
         Synchro_Joiner.Signal_End;
         while not My_SBoolean loop
            O ("task "
               & Integer'Image (Id)
               & " will wait");
            Wait (My_SCondition, My_SMutex);
         end loop;
         if J = Id then
            Counter.Increase;
         end if;
         O ("task "
            & Integer'Image (Id)
            & " will leave the mutex");
         Leave (My_SMutex);
      exception
         when Exc : others =>
            Ok := False;
            O ("task "
               & Integer'Image (Id)
               & " EXCEPTION RAISED ! "
               & Exception_Name (Exc)
               & " : "
               & Exception_Message (Exc));
      end;
      Synchro_Joiner.Signal_End;

      Synchro.Wait (3);
      --  Test that a call to Wait on a condition set to
      --  True is not blocking, and don't free the lock.
      O ("task "
         & Integer'Image (Id)
         & " will make a dumb test");
      begin
         Enter (My_SMutex);
         O ("task "
            & Integer'Image (Id)
            & " will wait");
         J := Id;
         while not My_SBoolean loop
            O ("task "
               & Integer'Image (Id)
               & " should not be here");
            pragma Assert (False);
            Wait (My_SCondition, My_SMutex);
         end loop;
         Ok := Ok and then (J = Id);
         O ("task "
            & Integer'Image (Id)
            & " will leave the mutex");
         Leave (My_SMutex);
      exception
         when Exc : others =>
            Ok := False;
            O ("task "
               & Integer'Image (Id)
               & "  EXCEPTION RAISED ! "
               & Exception_Name (Exc)
               & " : "
               & Exception_Message (Exc));
      end;
      Synchro_Joiner.Signal_End;

      Synchro.Wait (4);
      --  Test that a call to Wait outside a critical session
      --  raise  an exception.
      O ("task "
         & Integer'Image (Id)
         & " begins another dumb test");
      begin
         Ok := True;
         --  Wait (My_Monitor.all, My_Condition'Access);
         --  Should not be reached :
         --  Ok := False;
      exception
         when Exc : others =>
            O ("task "
               & Integer'Image (Id)
               & " EXCEPTION RAISED ! "
               & Exception_Name (Exc)
               & " : "
               & Exception_Message (Exc));
            null;
      end;
      Synchro_Joiner.Signal_End;

      Synchro.Wait (5);
      --  Test that the section after the wait is done in mutual exclusion
      O ("task "
         & Integer'Image (Id)
         & " begins test of mutual exclusion after wait ");
      begin
         Enter (My_SMutex);
         O ("task "
            & Integer'Image (Id)
            & " enters mutex ");
         Synchro_Joiner.Signal_End;
         while not My_SBoolean loop
            O ("task "
               & Integer'Image (Id)
               & " will wait");
            Wait (My_SCondition, My_SMutex);
         end loop;
         J := Id;
         Tempo;
         Ok := Ok and then (J = Id);
         O ("task "
            & Integer'Image (Id)
            & " will leave mutex ");
         Leave (My_SMutex);
      exception
         when Exc : others =>
            Ok := False;
            O ("task "
               & Integer'Image (Id)
               & " EXCEPTION RAISED ! "
               & Exception_Name (Exc)
               & " : "
               & Exception_Message (Exc));
      end;
      Synchro_Joiner.Signal_End;

      Synchro.Wait (6);
      --  Same test, but with a Wait condition already fulfilled
      O ("task "
         & Integer'Image (Id)
         & " will do a dumb test");
      begin
         Enter (My_SMutex);
         O ("task "
            & Integer'Image (Id)
            & " enters mutex");
         Synchro_Joiner.Signal_End;
         while not My_SBoolean loop
            O ("task "
               & Integer'Image (Id)
               & " will wait ");
            Wait (My_SCondition, My_SMutex);
         end loop;
         J := Id;
         Tempo;
         Ok := Ok and then (J = Id);
         O ("task "
            & Integer'Image (Id)
            & " will leave the mutex ");
         Leave (My_SMutex);
      exception
         when Exc : others =>
            Ok := False;
            O ("task "
               & Integer'Image (Id)
               & " EXCEPTION RAISED ! "
               & Exception_Name (Exc)
               & " : "
               & Exception_Message (Exc));
      end;
      Synchro.Wait (7);
      Synchro_Joiner.Signal_End;

   exception
      when Exc : others =>
         Ok := False;
         O ("task "
            & Integer'Image (Id)
            & " EXCEPTION RAISED ! "
            & Exception_Name (Exc)
            & " : "
            & Exception_Message (Exc));
   end Task_Test_Synchronisations;

   -----------------------
   -- Task_Test_Mutexes --
   -----------------------

   procedure Task_Test_Mutexes (Id : Integer) is
   begin
      Synchro.Simple_Wait;
      Synchro.Wait (1);
      --  Test that the mutual exclusion is assured by Adv Mutexes.
      begin
         PTMX.Enter (My_Mutex);
         J := Id;
         Tempo;
         Ok := Ok and then (J = Id);
         PTMX.Leave (My_Mutex);
      exception
         when Exc : others =>
            Ok := False;
            O ("task "
               & Integer'Image (Id)
               & " EXCEPTION RAISED ! "
               & Exception_Name (Exc)
               & " : "
               & Exception_Message (Exc));
      end;
      Synchro_Joiner.Signal_End;

      Synchro.Wait (2);
      --  Same test, except that the main task also try to get the mutex.
      begin
         PTMX.Enter (My_Mutex);
         J := Id;
         Tempo;
         Ok := Ok and then (J = Id);
         PTMX.Leave (My_Mutex);
      exception
         when Exc : others =>
            Ok := False;
            O ("task "
               & Integer'Image (Id)
               & " EXCEPTION RAISED ! "
               & Exception_Name (Exc)
               & " : "
               & Exception_Message (Exc));
      end;
      Synchro_Joiner.Signal_End;

      Synchro.Wait (3);
      --  Same test, except that the tasks enter several times in the critical
      --  section.
      begin
         PTMX.Enter (My_Mutex);
         J := Id;
         Tempo;
         Ok := Ok and then (J = Id);
         PTMX.Enter (My_Mutex);
         Tempo;
         Ok := Ok and then (J = Id);
         PTMX.Leave (My_Mutex);
         Tempo;
         Ok := Ok and then (J = Id);
         PTMX.Leave (My_Mutex);
      exception
         when Exc : others =>
            Ok := False;
            O ("task "
               & Integer'Image (Id)
               & " EXCEPTION RAISED ! "
               & Exception_Name (Exc)
               & " : "
               & Exception_Message (Exc));
      end;
      Synchro_Joiner.Signal_End;

   exception
      when Exc : others =>
         Ok := False;
         O ("task "
            & Integer'Image (Id)
            & " EXCEPTION RAISED ! "
            & Exception_Name (Exc)
            & " : "
            & Exception_Message (Exc));
   end Task_Test_Mutexes;

   -----------
   -- Tempo --
   -----------

   procedure Tempo (Time_In_Seconds : Float := Delay_Used) is
      use Ada.Real_Time;

      S : constant Time := Clock;
   begin
      delay until S + Milliseconds (Integer (1_000.0 * Time_In_Seconds));
   end Tempo;

   ------------
   -- Test_1 --
   ------------

   procedure Test_1 is
   begin
      Tempo;
      Counter.Increase;
      Tempo;
      Synchro_Joiner.Signal_End;
   exception
      when Exc : others =>
         O ("task test 1"
            & " EXCEPTION RAISED ! "
            & Exception_Name (Exc)
            & " : "
            & Exception_Message (Exc));
         null;
   end Test_1;

   Acc : PTT.Thread_Id;
   pragma Atomic (Acc);

   ------------
   -- Test_2 --
   ------------

   procedure Test_2 is
   begin
      Acc := Get_Current_Thread_Id (My_Thread_Factory);
      Tempo;
      if Acc = Get_Current_Thread_Id (My_Thread_Factory) then
         Counter.Increase;
      end if;
      Synchro_Joiner.Signal_End;
   exception
      when Exc : others =>
         O ("task test 2"
            & " EXCEPTION RAISED ! "
            & Exception_Name (Exc)
            & " : "
            & Exception_Message (Exc));
         Counter.Increase;
         Counter.Increase;
   end Test_2;

   ---------------------------
   -- Test_Synchronisations --
   ---------------------------

   procedure Test_Synchronisations is
      use PolyORB.Tasking.Mutexes;
      use PolyORB.Tasking.Condition_Variables;

   begin
      PolyORB.Utils.Report.New_Test ("Synchronisations");

      begin
         My_SBoolean := False;
         for J in Task_Index'Range loop
            Id_R (J) := new Identified_Runnable;
            Identified_Runnable (Id_R (J).all).Id := J;
            Identified_Runnable (Id_R (J).all).P  :=
              Task_Test_Synchronisations'Access;

            declare
               T : constant Thread_Access :=
                     Run_In_Task
                       (TF => My_Thread_Factory, R  => Id_R (J), Name => "T");
               pragma Unreferenced (T);
            begin
               null;
            end;
         end loop;
      exception
         when Exc : others =>
            O ("main task "
               & " EXCEPTION RAISED ! "
               & Exception_Name (Exc)
               & " : "
               & Exception_Message (Exc));
            Ok := False;
      end;
      PolyORB.Utils.Report.Output ("retest threads before synchro tests", Ok);

      Synchro.Reset;
      Counter.Reset;
      Synchro.Simple_Release;

      Synchro.Signal (1);
      Synchro_Joiner.Join;
      PolyORB.Utils.Report.Output ("test mutual exclusion", Ok);
      Ok := True;

      Synchro.Signal (2);
      Synchro_Joiner.Join;
      Enter (My_SMutex);
      My_SBoolean := True;
      Signal;
      Leave (My_SMutex);
      Synchro_Joiner.Join;
      Ok := Counter.Get = 1;
      PolyORB.Utils.Report.Output ("test the mutex is unlocked on Wait", Ok);
      Counter.Reset;
      Ok := True;

      Synchro.Signal (3);
      Synchro_Joiner.Join;
      PolyORB.Utils.Report.Output
        ("test that a Wait on True is not blocking", Ok);
      Counter.Reset;
      Ok := True;

      Synchro.Signal (4);
      Synchro_Joiner.Join;
      PolyORB.Utils.Report.Output
        ("test that Wait outside the monitor raise an exception",
         Ok);
      Counter.Reset;
      Ok := True;

      Enter (My_SMutex);
      My_SBoolean := False;
      Leave (My_SMutex);
      Synchro.Signal (5);
      Synchro_Joiner.Join;
      Enter (My_SMutex);
      My_SBoolean := True;
      Signal;
      Leave (My_SMutex);
      Synchro_Joiner.Join;
      PolyORB.Utils.Report.Output
        ("test the mutual exclusion after a Wait", Ok);
      Counter.Reset;
      Ok := True;

      Synchro.Signal (6);
      Synchro_Joiner.Join;
      Signal;
      Synchro.Signal (7);
      Synchro_Joiner.Join;
      PolyORB.Utils.Report.Output
        ("same test with a condition already fulfilled", Ok);
      Counter.Reset;
      Ok := True;

      Synchro.Reset_Simple_Wait;
   end Test_Synchronisations;

   ------------------
   -- Test_Mutexes --
   ------------------

   procedure Test_Mutexes is
      Id : constant Integer := -1;
   begin
      PolyORB.Utils.Report.New_Test ("Mutexes");

      PTMX.Create (My_Mutex);
      Synchro.Reset;
      Counter.Reset;

      begin
         for J in Task_Index'Range loop
            Id_R (J) := new Identified_Runnable;
            Identified_Runnable (Id_R (J).all).Id := J;
            Identified_Runnable (Id_R (J).all).P
              := Task_Test_Mutexes'Access;

            declare
               T : constant Thread_Access :=
                     Run_In_Task
                       (TF => My_Thread_Factory, R  => Id_R (J), Name => "T");
               pragma Unreferenced (T);
            begin
               null;
            end;
         end loop;
      exception
         when Exc : others =>
            O ("main task "
               & " EXCEPTION RAISED ! "
               & Exception_Name (Exc)
               & " : "
               & Exception_Message (Exc));
            Ok := False;
      end;
      PolyORB.Utils.Report.Output
        ("retest threads before Adv mutex tests", Ok);

      Synchro.Simple_Release;
      Synchro.Signal (1);
      Synchro_Joiner.Join;

      PolyORB.Utils.Report.Output ("test mutual exclusion for Adv mutex", Ok);

      Ok := True;

      Synchro.Signal (2);

      begin
         PTMX.Enter (My_Mutex);
         J := Id;
         Tempo;
         Ok := Ok and then (J = Id);
         PTMX.Leave (My_Mutex);
      exception
         when Exc : others =>
            O ("main task "
               & " EXCEPTION RAISED ! "
               & Exception_Name (Exc)
               & " : "
               & Exception_Message (Exc));
            Ok := False;
      end;

      Synchro_Joiner.Join;
      PolyORB.Utils.Report.Output
        ("same test, with the main task involved", Ok);
      Ok := True;

      Synchro.Signal (3);

      begin
         PTMX.Enter (My_Mutex);
         J := Id;
         Tempo;
         Ok := Ok and then (J = Id);
         PTMX.Enter (My_Mutex);
         Tempo;
         Ok := Ok and then (J = Id);
         PTMX.Leave (My_Mutex);
         Tempo;
         Ok := Ok and then (J = Id);
         PTMX.Leave (My_Mutex);
      exception
         when Exc : others =>
            Ok := False;
            O ("main task "
               & " EXCEPTION RAISED ! "
               & Exception_Name (Exc)
               & " : "
               & Exception_Message (Exc));
      end;

      Synchro_Joiner.Join;
      PolyORB.Utils.Report.Output
        ("same test, with Enter called several times", Ok);
      Ok := True;

   exception
      when Exc : others =>
         Ok := False;
         O ("main task "
            & " EXCEPTION RAISED ! "
            & Exception_Name (Exc)
            & " : "
            & Exception_Message (Exc));
         raise;
   end Test_Mutexes;

   ------------------
   -- Test_Threads --
   ------------------

   procedure Test_Threads is
      Thr_Ok : Boolean;
   begin
      PolyORB.Utils.Report.New_Test ("Thread manipulation");

      Synchro.Reset;
      Counter.Reset;

      begin
         for J in Task_Index'Range loop
            R (J) := new Generic_Run;
            Generic_Run (R (J).all).Id := J;
            Generic_Run (R (J).all).P := Test_1'Access;

            declare
               T : constant Thread_Access :=
                     Run_In_Task
                       (TF => My_Thread_Factory, R  => R (J), Name => "T");
               pragma Unreferenced (T);
            begin
               null;
            end;
         end loop;
         Thr_Ok := True;
      exception
         when Exc : others =>
            Thr_Ok := False;
            O ("main task "
               & " EXCEPTION RAISED ! "
               & Exception_Name (Exc)
               & " : "
               & Exception_Message (Exc));
      end;
      Synchro_Joiner.Join;
      Thr_Ok := (Counter.Get = Number_Of_Tasks) and then Thr_Ok;
      PolyORB.Utils.Report.Output
        ("test that the expected number of tasks is created", Thr_Ok);

      Counter.Reset;

      begin
         for J in Task_Index'Range loop
            R (J) := new Generic_Run;
            Generic_Run (R (J).all).Id := J;
            Generic_Run (R (J).all).P := Test_2'Access;

            declare
               T : constant Thread_Access :=
                     Run_In_Task
                       (TF => My_Thread_Factory, R  => R (J), Name => "T");
               pragma Unreferenced (T);
            begin
               null;
            end;
         end loop;
         Synchro_Joiner.Join;
         Thr_Ok := True;
      exception
         when Exc : others =>
            Thr_Ok := False;
            O ("main task "
               & " EXCEPTION RAISED ! "
               & Exception_Name (Exc)
               & " : "
               & Exception_Message (Exc));
      end;
      Thr_Ok := (Counter.Get >= 1) and then Thr_Ok;
      PolyORB.Utils.Report.Output ("test Get_Current_Thread",
                                   Thr_Ok);
   end Test_Threads;

end Test000_Common;
