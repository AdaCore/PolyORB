------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--               POLYORB.TASKING.PROFILES.RAVENSCAR.THREADS                 --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--         Copyright (C) 2002-2003 Free Software Foundation, Inc.           --
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

--  Abstraction types for Ravenscar tasking.

--  Under the Ravenscar profile, "Threads" are associated with an
--  unique synchronisation object which is the only one they can wait
--  on. This assures that only one task wait on every entry, as
--  required in the Ravenscar profile.

--  The child packages of this package should only be packages
--  providing tasking facilities. Other packages shoud not have access
--  to "suspend" and "resume", the procedures affecting the internal
--  synchronisation object.

--  XXX inconsistant, "suspend" and "resume" are used by CV and mutexes !!!

with System;

with PolyORB.Tasking.Threads;
with PolyORB.Tasking.Profiles.Ravenscar.Index_Manager;

generic
   Number_Of_Threads : Integer;
   Task_Priority     : System.Priority;
package PolyORB.Tasking.Profiles.Ravenscar.Threads is

   pragma Elaborate_Body;

   use PolyORB.Tasking.Threads;

   package PTT renames PolyORB.Tasking.Threads;

   --  Ravenscar tasking profile.
   --  The documentation for the following declarations can be
   --  found in PolyORB.Tasking.Threads.

   type Ravenscar_Thread_Type is new Thread_Type with private;

   type Ravenscar_Thread_Access is access all Ravenscar_Thread_Type'Class;

   function Get_Thread_Id (T : access Ravenscar_Thread_Type)
     return Thread_Id;

   type Ravenscar_Thread_Factory_Type is
     new Thread_Factory_Type with private;

   type Ravenscar_Thread_Factory_Access is
     access all Ravenscar_Thread_Factory_Type'Class;

   The_Thread_Factory : constant Ravenscar_Thread_Factory_Access;
   --  The thread factory for this profile.

   function Run_In_Task
     (TF               : access Ravenscar_Thread_Factory_Type;
      Name             : String := "";
      Default_Priority : System.Any_Priority := System.Default_Priority;
      R                : Runnable_Access;
      C                : Runnable_Controller_Access)
     return Thread_Access;

   function Run_In_Task
     (TF               : access Ravenscar_Thread_Factory_Type;
      Name             : String := "";
      Default_Priority : System.Any_Priority := System.Default_Priority;
      P                : Parameterless_Procedure)
     return Thread_Access;

   function Get_Current_Thread_Id
     (TF : access Ravenscar_Thread_Factory_Type)
     return Thread_Id;

   function Thread_Id_Image
     (TF : access Ravenscar_Thread_Factory_Type;
      TID : PTT.Thread_Id)
     return String;

   procedure Set_Priority
     (TF : access Ravenscar_Thread_Factory_Type;
      T  :        PTT.Thread_Id;
      P  :        System.Any_Priority);
   pragma No_Return (Set_Priority);
   --  Setting priority has no meaning under this profile,
   --  raise PolyORB.Tasking.Tasking_Profile_Error.

   function Get_Priority
     (TF : access Ravenscar_Thread_Factory_Type;
      T  :        PTT.Thread_Id)
     return System.Any_Priority;
   --  XXX not (yet) implemented,
   --  raise PolyORB.Tasking.Tasking_Profile_Error.

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
     (Number_Of_Threads + 3);
   --  XXX + 3 is a temporary workaround for thet leak of Sync objects.

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
   --  Prepare_Wait were done before the call to Resume),
   --  the signal is lost.

   function Get_Thread_Index (T : Thread_Id) return Integer;
   --  Return a different integer for each Thread_Id.

private

   type Ravenscar_Thread_Factory_Type is new Thread_Factory_Type
     with null record;

   The_Thread_Factory : constant Ravenscar_Thread_Factory_Access
     := new Ravenscar_Thread_Factory_Type;

   type Ravenscar_Thread_Type is new Thread_Type with record
      Id      : PTT.Thread_Id;
      --  Id of the Thread.

      Sync_Id : Synchro_Index_Type;
      pragma Atomic (Sync_Id);
      --  if the thread is available to be allocated to a caller of
      --  Run_In_Task, Sync_Id is the Id of the Synchro on which the
      --  corresponding task is waiting.
   end record;

   procedure Initialize;
   --  Package Initialization procedure.

end PolyORB.Tasking.Profiles.Ravenscar.Threads;
