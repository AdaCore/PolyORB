------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--             P O L Y O R B . T A S K I N G . P R O F I L E S              --
--                   . R A V E N S C A R . T H R E A D S                    --
--                                                                          --
--                                 S p e c                                  --
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

--  Abstraction types for Ravenscar tasking.

--  Under the Ravenscar profile, "Threads" are
--  associated with an unique synchronisation
--  object on which is the only one they can wait on.
--  This assures that only one task wait on every
--  entry, as required in the Ravenscar profile.

--  The child packages of this package should only
--  be packages providing tasking facilities. Other
--  packages shoud not have access to "suspend" and "resume",
--  the procedures affecting the internal
--  synchronisation object.

with PolyORB.Tasking.Threads;
with System;

package PolyORB.Tasking.Profiles.Ravenscar.Threads is
   pragma Elaborate_Body;

   use PolyORB.Tasking.Threads;

   --  Ravenscar tasking profile.
   --  The documentation for the following declarations can be
   --  found in PolyORB.Tasking.Threads.

   type Ravenscar_Thread_Id is new Thread_Id with private;

   type Ravenscar_Thread_Id_Access is access all Ravenscar_Thread_Id'Class;

   function "="
     (T1 : Ravenscar_Thread_Id;
      T2 : Ravenscar_Thread_Id)
     return Boolean;

   function Image (T : Ravenscar_Thread_Id) return String;

   type Ravenscar_Thread_Type is new Thread_Type with private;

   type Ravenscar_Thread_Access is access all Ravenscar_Thread_Type'Class;

   function Get_Thread_Id (T : access Ravenscar_Thread_Type)
     return Thread_Id_Access;

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
      R                : Runnable'Class)
     return Thread_Access;

   function Run_In_Task
     (TF               : access Ravenscar_Thread_Factory_Type;
      Name             : String := "";
      Default_Priority : System.Any_Priority := System.Default_Priority;
      P                : Parameterless_Procedure)
     return Thread_Access;

   procedure Set_Priority
     (TF : access Ravenscar_Thread_Factory_Type;
      T  : Thread_Id'Class;
      P  : System.Any_Priority);
   --  This function has no sense in Ravenscar profile,
   --  It simply raises a Tasking.Tasking_Profile_Error.

   function Get_Current_Thread_Id
     (TF : access Ravenscar_Thread_Factory_Type)
     return Thread_Id'Class;

   procedure Copy_Thread_Id
     (TF     : access Ravenscar_Thread_Factory_Type;
      Source : Thread_Id'Class;
      Target : Thread_Id_Access);

   --  The following procedures make access to the
   --  internal synchronisation objects, so it should
   --  only be used by other packages that thread pool ones,
   --  and synchronisations.
   --  We have two different types of operation : the deterministic
   --  and non-deterministic ones. They play the same role, but
   --  the deterministic one are to use in tagged types/packages that assure
   --  a deterministic delay for the execution of its procedures.
   --  A mutex with a bounded queue is a good example.
   --
   --  Semantics:

   --  (the following rules applies also for deterministic
   --  procedures)
   --
   --  A thread has three states : Prepared, Waiting, Free.  It is
   --  initialy Free.
   --  If it is Free, it can become Prepared after a call to Prepare_Suspend.
   --  If it is Prepared, it can become Waiting after a call to Suspend,
   --  or it can become Free by a call to Prepare_Suspend (False).
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
   --  prepare_suspend (False)
   --
   --  3:
   --  prepare_suspend
   --  prepare_suspend (False)
   --  prepare_suspend
   --  suspend

   --  These one will raise an assertion failure :
   --  (From Free)
   --
   --  1:
   --  suspend
   --
   --  2:
   --  prepare_suspend (False)
   --
   --  3:
   --  prepare_suspend
   --  prepare_suspend (False)
   --  prepare_suspend (False)

   --  XXX Note that the use of two-phases suspends (Prepare_Suspend - Suspend)
   --  is more a way to check the integrity of the Ravenscar profile that
   --  a real need of the synchronisations objects.
   --  It may disappear in the future.

   procedure Prepare_Suspend
     (T     : Ravenscar_Thread_Id;
      State : Boolean := True);
   --  This procedure registers thread-safely the task given in
   --  parameter as a suspending task. It MUST be called before a
   --  corresponding Suspend.
   --  If State = False, it abort the previous call to Prepare_Suspend.

   procedure Prepare_Deterministic_Suspend
     (T    : Ravenscar_Thread_Id;
     State : Boolean := True);
   --  This procedure is only used for deterministic suspension:
   --  for example, the wait in a bounded FIFO for
   --  a mutex.
   --  The same conditions that Prepare_Suspend applies.

   procedure Suspend (T : Ravenscar_Thread_Id);
   --  Calling this procedure, the current task awaits on the internal
   --  synchronisation. The task that calls Suspend MUST have called
   --  Prepare_Suspend before; Otherwise, it will raise an assertion.
   --  The calling task MUST be the one which abstraction
   --  is "T". Otherwise, it would wait on a synchronisation
   --  object that doesn't belong to her, which would
   --  raise a Program_Error if another task call
   --  "Suspend" on the same synchronisation object.

   procedure Deterministic_Suspend (T : Ravenscar_Thread_Id);
   --  This procedure is only used for deterministic suspension:
   --  for example, the wait in a bounded FIFO for
   --  a mutex.
   --  The same conditions that Suspend applies.

   procedure Resume (T : Ravenscar_Thread_Id);
   --  The call to this procedure free the task waiting
   --  on the internal synchronisation object of "T".
   --  If no task is about to Wait (that is, if no call to
   --  Prepare_Wait were done before the call to Resume),
   --  the signal is lost.

   procedure Deterministic_Resume (T : Ravenscar_Thread_Id);
   --  The call to this procedure free the task waiting
   --  on the internal synchronisation object of "T" dedicated
   --  to the deterministic suspension.

   function Get_Thread_Index (T : Ravenscar_Thread_Id)
                             return Integer;
   --  return a different integer for each Thread_Id.

   procedure Initialize;
   --  Initialize the package.

private

   type Ravenscar_Thread_Id is new Thread_Id with record
      Id : Integer;
   end record;

   type Ravenscar_Thread_Factory_Type is new Thread_Factory_Type with record
        null;
   end record;

   The_Thread_Factory : constant Ravenscar_Thread_Factory_Access
     := new Ravenscar_Thread_Factory_Type;

   type Ravenscar_Thread_Type is new Thread_Type with record
      Id   : aliased Ravenscar_Thread_Id;
      --  Id of the Thread.

   end record;

end PolyORB.Tasking.Profiles.Ravenscar.Threads;
