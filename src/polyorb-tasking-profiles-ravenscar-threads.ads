------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--               POLYORB.TASKING.PROFILES.RAVENSCAR.THREADS                 --
--                                                                          --
--                                 S p e c                                  --
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

pragma Ada_2005;

--  Abstraction types for Ravenscar tasking.

--  Under the Ravenscar profile, "Threads" are associated with an
--  unique synchronisation object which is the only one they can wait
--  on. This assures that only one task wait on every entry, as
--  required in the Ravenscar profile.

with System;
pragma Warnings (Off);
--  System.Tasking is an internal GNAT unit
with System.Tasking;
pragma Warnings (On);

with PolyORB.Initialization;

with PolyORB.Tasking.Threads;
with PolyORB.Tasking.Profiles.Ravenscar.Index_Manager;

generic
   Number_Of_Application_Tasks : Integer;
   --  Number of tasks created by the user

   Number_Of_System_Tasks      : Integer;
   --  Number of tasks created by the PolyORB run-time library

   Task_Priority               : System.Priority;
   --  Priority of the system tasks

   Storage_Size                : Natural;
   --  Stack size of the system tasks

package PolyORB.Tasking.Profiles.Ravenscar.Threads is

   pragma Elaborate_Body;

   use PolyORB.Tasking.Threads;

   package PTT renames PolyORB.Tasking.Threads;

   --  Ravenscar tasking profile

   --  The documentation for the following declarations can be
   --  found in PolyORB.Tasking.Threads.

   type Ravenscar_Thread_Type is new Thread_Type with private;

   type Ravenscar_Thread_Access is access all Ravenscar_Thread_Type'Class;

   overriding function Get_Thread_Id (T : access Ravenscar_Thread_Type)
     return Thread_Id;

   type Ravenscar_Thread_Factory_Type is
     new Thread_Factory_Type with private;

   type Ravenscar_Thread_Factory_Access is
     access all Ravenscar_Thread_Factory_Type'Class;

   The_Thread_Factory : constant Ravenscar_Thread_Factory_Access;

   overriding function Run_In_Task
     (TF               : access Ravenscar_Thread_Factory_Type;
      Name             : String;
      Default_Priority : System.Any_Priority := System.Default_Priority;
      Storage_Size     : Natural := 0;
      R                : Runnable_Access) return Thread_Access;

   overriding function Run_In_Task
     (TF               : access Ravenscar_Thread_Factory_Type;
      Name             : String;
      Default_Priority : System.Any_Priority := System.Default_Priority;
      Storage_Size     : Natural := 0;
      P                : Parameterless_Procedure) return Thread_Access;

   overriding function Get_Current_Thread_Id
     (TF : access Ravenscar_Thread_Factory_Type)
     return Thread_Id;

   overriding function Thread_Id_Image
     (TF : access Ravenscar_Thread_Factory_Type;
      TID : PTT.Thread_Id)
     return String;

   overriding procedure Set_Priority
     (TF : access Ravenscar_Thread_Factory_Type;
      T  :        PTT.Thread_Id;
      P  :        System.Any_Priority);
   pragma No_Return (Set_Priority);
   --  Setting priority has no meaning under this profile, raise Tasking_Error

   overriding function Get_Priority
     (TF : access Ravenscar_Thread_Factory_Type;
      T  :        PTT.Thread_Id)
     return System.Any_Priority;

   overriding procedure Relative_Delay
     (TF : access Ravenscar_Thread_Factory_Type; D : Duration);

   overriding function Awake_Count (TF : access Ravenscar_Thread_Factory_Type)
     return Natural;

   overriding function Independent_Count
     (TF : access Ravenscar_Thread_Factory_Type)
     return Natural;

   -------------------------------------------------
   --  Ravenscar specific synchronization objects --
   -------------------------------------------------

   --  The following procedures make access to the
   --  profile-specific synchronisation objects, so it should
   --  only be used by other packages that thread pool ones,
   --  and synchronisations.

   --  Semantics:

   --  A thread has three states : Prepared, Waiting, Free.  It is
   --  initialy Free.
   --  If it is Free, it can become Prepared after a call to Prepare_Suspend.
   --  If it is Prepared, it can become Waiting after a call to Suspend,
   --  or it can become Free by a call to Abort_Suspend.
   --  If it is Waiting, it can become Free by a call (by another thread)
   --  to Resume.
   --  Any other transition makes no sense, and will raise an Assertion
   --  failure, which will (most likely) be a bug in the Ravenscar profile.
   --
   --  To illustrate it, those typical sequences are authorized
   --  (from the state Free):
   --
   --  1:
   --  prepare_suspend
   --  suspend
   --
   --
   --  2:
   --  prepare_suspend
   --  abort_suspend
   --
   --  3:
   --  prepare_suspend
   --  abort_suspend
   --  prepare_suspend
   --  suspend

   --  These one will raise an assertion failure :
   --  (From Free)
   --
   --  1:
   --  suspend
   --
   --  2:
   --  abort_suspend
   --
   --  3:
   --  prepare_suspend
   --  abort_suspend
   --  abort_suspend

   package Synchro_Index_Manager is
      new PolyORB.Tasking.Profiles.Ravenscar.Index_Manager
     (Number_Of_System_Tasks + Number_Of_Application_Tasks);
   --  The number of synchronization objects is the maximum number of
   --  tasks. Note that if a task have a synchronization object handle
   --  and it may NOT be blocked; this mean that if all the tasks have
   --  an handle, it is not an error per se.

   type Synchro_Index_Type is new Synchro_Index_Manager.Index_Type;
   --  A Synchro_Index_Type represents an index in a pool of synchro objects.
   --  The synchro objects are managed by pools, and are reallocated
   --  at every call to a suspension functionality.

   function Prepare_Suspend return Synchro_Index_Type;
   --  This function registers thread-safely the current task
   --  as a suspending task. It MUST be called before a
   --  corresponding Suspend.

   procedure Abort_Suspend (S : Synchro_Index_Type);
   --  This function abort the previous call to Prepare_Suspend.

   procedure Suspend (S : Synchro_Index_Type);
   --  Calling this procedure, the current task awaits on S (that is,
   --  wait that another thread call Resume on S). The task that calls
   --  Suspend MUST have called Prepare_Suspend before; Otherwise, it
   --  will raise an assertion.

   procedure Resume (S : Synchro_Index_Type);
   --  The call to this procedure free the task waiting
   --  on S.
   --  If no task is about to Wait (that is, if no call to
   --  Prepare_Suspend were done before the call to Resume),
   --  the signal is lost.

   function Get_Thread_Index (T : Thread_Id) return Integer;
   --  Return a different integer for each Thread_Id

private

   type Ravenscar_Thread_Factory_Type is new Thread_Factory_Type
   with record
      Environment_Task : System.Tasking.Task_Id;
      --  The environment task
   end record;

   The_Thread_Factory : constant Ravenscar_Thread_Factory_Access
     := new Ravenscar_Thread_Factory_Type;

   type Ravenscar_Thread_Type is new Thread_Type with record
      Id      : PTT.Thread_Id;
      --  Id of the Thread

      Sync_Id : Synchro_Index_Type;
      pragma Atomic (Sync_Id);
      --  if the thread is available to be allocated to a caller of
      --  Run_In_Task, Sync_Id is the Id of the Synchro on which the
      --  corresponding task is waiting.
   end record;

   procedure Initialize;

   Initializer : constant PolyORB.Initialization.Initializer :=
                   Initialize'Access;

end PolyORB.Tasking.Profiles.Ravenscar.Threads;
