------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--             P O L Y O R B . T A S K I N G . P R O F I L E S              --
--                   . R A V E N S C A R . T H R E A D S                    --
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

--  Implementation of Threads under the Ravenscar profile.

with Ada.Task_Identification;
with PolyORB.Initialization;
with PolyORB.Utils.Strings;
with PolyORB.Log;

package body PolyORB.Tasking.Profiles.Ravenscar.Threads is

   use PolyORB.Log;

   package PTT renames PolyORB.Tasking.Threads;

   package L is new PolyORB.Log.Facility_Log
     ("polyorb.tasking.profiles.ravenscar.threads");

   procedure O (Message : in String; Level : Log_Level := Debug)
     renames L.Output;

   Initialized : Boolean := False;
   --  whether or not the package is initialized.

   ---------
   -- Ids --
   ---------

   --  Id are associated to the tasks created from this package.
   --  The last Id is reserved to the main context, which
   --  is the task that execute the Initialize procedure of this package.
   --  In this package, it is called the main task, and the Thread object
   --  associated to it is called the main Thread.

   --  paramaters associated to this main task :

   Main_Task_Id     : constant Integer := Thread_Index_Type'Last;

   Main_Task_Tid    : Ada.Task_Identification.Task_Id;

   -------------------
   -- Tasking Types --
   -------------------

   --  Tasking type used in the pools preallocated by this package:

   task type Simple_Task;
   --  Type of the task that run the submitted threads
   --  XXX We should use a facade package to some pools.
   --      The pool packages will generated from the configuration
   --      file.

   protected type Barrier is
      --  Type of the internal synchronisation object of the tasks
      --  allocated through this package.
      --  A call to Suspend will result in a call to Wait;
      --  a call to Resume will result in a call to Signal.

      procedure Prepare_Wait (State : Boolean);
      --  If State:= True, initialize the barrier for a call to
      --  Wait. If State is set to False, it abort the previous
      --  call to Prepare_Wait.

      entry Wait;
      --  Wait until it is signaled.

      procedure Signal;
      --  Signal the Suspension_Object.

      function Get_Waiting return Boolean;

      function Get_Signaled return Boolean;

   private
      Signaled : Boolean := False;
      Waiting  : Boolean := False;
   end Barrier;

   -----------
   -- Pools --
   -----------

   --  Every object used in this package is preallocated at initialisation
   --  time, in a pool.

   type Task_Id_Arr is array (Thread_Index_Type)
     of Ada.Task_Identification.Task_Id;
   --  Table of the Task_Id of the task of the pool.

   protected Pool_Manager is
      --  Protected manager for the pool.

      procedure Initialize_Id
        (Tid : Ada.Task_Identification.Task_Id;
         Id  : out Thread_Index_Type);
      --  This procedure is called at initialization time
      --  by the tasks, to get a unique id.

      procedure End_Initialization (Id : Thread_Index_Type);
      --  Signal that the task which id is Id has finished its initialization

      entry Wait_For_Package_Initialization;
      --  This entry block the initialisation loop
      --  until all the Thread are initialised.

      procedure Create_Thread
        (Id  : Thread_Index_Type;
         Run : Runnable_Access;
         C   : Runnable_Controller_Access;
         T   : out Thread_Access);
      --  This is the protected section of the Create_Thread procedure.

      procedure Create_Thread
        (Id : Thread_Index_Type;
         P  : Parameterless_Procedure;
         T  : out Thread_Access);
      --  This is the protected section of the Create_Thread procedure.

      procedure Lookup (Tid : Ada.Task_Identification.Task_Id;
                        Result : out Integer);
      --  Get the Thread_Access associated to the given Task_Id.

      procedure Initialize;
      --  Initialisation procedure of Pool_Manager.

   private
      Package_Initialized : Boolean := False;
      Current             : Integer := Task_Index_Type'First;
      My_Task_Id_Arr      : Task_Id_Arr;
   end Pool_Manager;

   type Thread_Arr is array (Thread_Index_Type)
     of aliased Ravenscar_Thread_Type;

   My_Thread_Arr  : Thread_Arr;
   --  Pool of Threads.

   type Runnable_Arr is array (Thread_Index_Type)
     of Runnable_Access;

   type Controller_Arr is array (Thread_Index_Type)
     of Runnable_Controller_Access;

   type Job_Passing is (Use_Runnable, Use_PP);
   --  There is two way to pass a job to the tasks:
   --  by a Runnable or by a Parameterless_Procedure.
   --  This type is used to discriminate.

   type Job_Passing_Arr is array (Thread_Index_Type)
     of Job_Passing;

   My_Job_Passing_Arr : Job_Passing_Arr;

   My_Runnable_Arr : Runnable_Arr;
   --  Pool of Runnable.

   My_Controller_Arr : Controller_Arr;
   --  Pool of Runnable_Controller.

   type PP_Arr is array (Thread_Index_Type)
     of Parameterless_Procedure;

   My_PP_Arr : PP_Arr;
   --  Pool of Parameterless_Procedure.

   Task_Pool : array (Task_Index_Type) of Simple_Task;
   pragma Warnings (Off);
   pragma Unreferenced (Task_Pool);
   pragma Warnings (On);
   --  Pool of preallocated tasks.

   type Barrier_Arr is array (Synchro_Index_Type)
     of Barrier;

   Sync_Pool : Barrier_Arr;
   --  Pool of Barrier used for synchronisations.

   ----------
   -- Free --
   ----------

   --  procedure Free is new Unchecked_Deallocation
   --  (Runnable_Controller'Class, Runnable_Controller_Access);
   --  XXX to suppress soon...

   ---------
   -- "=" --
   ---------

   function "="
     (T1 : Ravenscar_Thread_Id;
      T2 : Ravenscar_Thread_Id)
     return Boolean is
   begin
      pragma Assert (Initialized);
      return T1.Id = T2.Id;
   end "=";

   -------------------
   -- Abort_Suspend --
   -------------------

   procedure Abort_Suspend (S : Synchro_Index_Type) is
   begin
      pragma Assert (Initialized);
      pragma Debug (O ("abort suspend on " & Integer'Image (Integer (S))));
      Sync_Pool (S).Prepare_Wait (False);
      pragma Debug (O ("abort done on " & Integer'Image (Integer (S))));
      Synchro_Index_Manager.Release (Synchro_Index_Manager.Index_Type (S));
   end Abort_Suspend;

   -------------
   -- Barrier --
   -------------

   protected body Barrier is

      function Get_Waiting return Boolean is
      begin
         return Waiting;
      end Get_Waiting;

      function Get_Signaled return Boolean is
      begin
         return Signaled;
      end Get_Signaled;

      --------------------------
      -- Barrier.Prepare_Wait --
      --------------------------

      procedure Prepare_Wait (State : Boolean) is
      begin
         pragma Assert (not Signaled);
         --  Why should we be signaled if we are not waiting yet?
         --  It would definitely be an error.
         pragma Assert (State or Waiting);
         --  Fail if we try to abort, but no call to suspend were prepared.
         pragma Assert (not State or not Waiting);
         --  Fail if it is the second call to Prepare_Wait (True)
         Waiting := State;
      end Prepare_Wait;

      --------------------
      -- Barrier.Signal --
      --------------------

      procedure Signal is
      begin
         pragma Assert (not Signaled);
         --  XXX This assertion is a temporary one; it is just to see
         --  if some signal are lost.  If it is raised in one of your
         --  tests, comment this line and tell me (JG) how you this
         --  assertion was raised.
         --  Received two signals before being released. One will be lost.
         --  Is it a normal behaviour? It should be the reponsibility of
         --  the synchro objects (Mutexes, CV) to take care of this loss,
         --  not the thread's.

         pragma Assert (Waiting);
         --  XXX This assertion is a temporary one; it is just to see
         --  if some signal are lost.  If it is raised in one of your
         --  tests, comment this line and tell me (JG) how this
         --  assertion was raised.

         if Waiting then
            Signaled := True;
         end if;
      end Signal;

      ------------------
      -- Barrier.Wait --
      ------------------

      entry Wait when Signaled is
      begin
         pragma Assert (Waiting);
         --  Error : Prepare_Wait have not been called before.
         pragma Debug (O ("wait done!"));
         Signaled := False;
         Waiting := False;
      end Wait;

   end Barrier;

   --------------------
   -- Copy_Thread_Id --
   --------------------

   procedure Copy_Thread_Id
     (TF     : access Ravenscar_Thread_Factory_Type;
      Source : Thread_Id'Class;
      Target : Thread_Id_Access) is
      pragma Warnings (Off);
      pragma Unreferenced (TF);
      pragma Warnings (On);
      Result  : constant Ravenscar_Thread_Id_Access
        := Ravenscar_Thread_Id_Access (Target);
      Content : constant Ravenscar_Thread_Id
        := Ravenscar_Thread_Id (Source);
   begin
      pragma Assert (Initialized);
      Result.Id := Content.Id;
   end Copy_Thread_Id;

   ---------------------------
   -- Get_Current_Thread_Id --
   ---------------------------

   function Get_Current_Thread_Id
     (TF : access Ravenscar_Thread_Factory_Type)
     return Thread_Id'Class is
      pragma Warnings (Off);
      pragma Unreferenced (TF);
      pragma Warnings (On);
      Task_Id : constant Ada.Task_Identification.Task_Id
        := Ada.Task_Identification.Current_Task;
      Result  : Ravenscar_Thread_Id;
   begin
      pragma Assert (Initialized);
      Pool_Manager.Lookup (Task_Id, Result.Id);
      return Thread_Id'Class (Result);
   end Get_Current_Thread_Id;

   -------------------
   -- Get_Thread_Id --
   -------------------

   function Get_Thread_Id (T : access Ravenscar_Thread_Type)
                          return Thread_Id_Access is
   begin
      pragma Assert (Initialized);
      return T.Id'Access;
   end Get_Thread_Id;

   ----------------------
   -- Get_Thread_Index --
   ----------------------

   function Get_Thread_Index (T : Ravenscar_Thread_Id)
                             return Integer is
   begin
      pragma Assert (Initialized);
      return T.Id;
   end Get_Thread_Index;

   -----------
   -- Image --
   -----------

   function Image (T : Ravenscar_Thread_Id) return String is
   begin
      pragma Assert (Initialized);
      return Integer'Image (T.Id);
   end Image;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is
   begin
      pragma Assert (not Initialized);
      Thread_Index_Manager.Initialize;
      Synchro_Index_Manager.Initialize (False);
      Main_Task_Tid := Ada.Task_Identification.Current_Task;
      Pool_Manager.Initialize;
      PTT.Register_Thread_Factory (PTT.Thread_Factory_Access
                                   (The_Thread_Factory));
      Pool_Manager.Wait_For_Package_Initialization;
      Initialized := True;
   end Initialize;

   protected Stopper is

      procedure Can_Stop (B : out Boolean);

      entry Stop;

   private
      My : Boolean := True;
   end Stopper;

   protected body Stopper is

      procedure Can_Stop (B : out Boolean) is
      begin
         B := My;
         My := False;
      end Can_Stop;

      entry Stop when False is
      begin
         null;
      end Stop;

   end Stopper;

   ---------------------
   -- Prepare_Suspend --
   ---------------------

   function Prepare_Suspend return Synchro_Index_Type is
      S : Synchro_Index_Type;
      B : Boolean;
   begin
      Synchro_Index_Manager.Get (Synchro_Index_Manager.Index_Type (S));
      pragma Debug (O ("prepare a suspend on " & Integer'Image (Integer (S))));
      Sync_Pool (S).Prepare_Wait (True);
      pragma Debug (O ("suspend prepared on " & Integer'Image (Integer (S))));
      return S;
   exception
      when others =>
         Stopper.Can_Stop (B);
         if B then
            Stopper.Stop;
         end if;
         raise;
   end Prepare_Suspend;

   ------------------
   -- Pool_Manager --
   ------------------

   protected body Pool_Manager is

      --------------------------------
      -- Pool_Manager.Create_Thread --
      --------------------------------

      procedure Create_Thread
        (Id  : Thread_Index_Type;
         Run : Runnable_Access;
         C   : Runnable_Controller_Access;
         T   : out Thread_Access) is
         Result : Ravenscar_Thread_Access;
      begin
         pragma Assert (Package_Initialized);
         My_Job_Passing_Arr (Id) := Use_Runnable;
         My_Runnable_Arr (Id) := Run;
         My_Controller_Arr (Id) := C;
         Result := My_Thread_Arr (Id)'Access;
         T := Thread_Access (Result);
      end Create_Thread;

      procedure Create_Thread
        (Id : Thread_Index_Type;
         P  : Parameterless_Procedure;
         T  : out Thread_Access) is
         Result : Ravenscar_Thread_Access;
      begin
         pragma Assert (Package_Initialized);
         My_Job_Passing_Arr (Id) := Use_PP;
         My_PP_Arr (Id) := P;
         Result := My_Thread_Arr (Id)'Access;
         T := Thread_Access (Result);
      end Create_Thread;

      ------------------------
      -- End_Initialization --
      ------------------------

      procedure End_Initialization (Id : Thread_Index_Type) is
         pragma Warnings (Off);
         pragma Unreferenced (Id);
         pragma Warnings (On);
      begin
         Package_Initialized := Current >= Thread_Index_Type'Last;
      end End_Initialization;

      -----------------------------
      -- Pool_Manager.Initialize --
      -----------------------------

      procedure Initialize is
      begin
         My_Task_Id_Arr (Main_Task_Id) := Main_Task_Tid;
         My_Thread_Arr (Main_Task_Id).Id.Id := Main_Task_Id;
      end Initialize;

      --------------------------------
      -- Pool_Manager.Initialize_Id --
      --------------------------------

      procedure Initialize_Id
        (Tid : Ada.Task_Identification.Task_Id;
         Id  : out Thread_Index_Type) is
      begin
         pragma Assert (not Package_Initialized);
         pragma Assert (Current <= Thread_Index_Type'Last);
         Id := Current;
         My_Thread_Arr (Id).Id.Id := Id;
         My_Task_Id_Arr (Current) := Tid;
         Current := Current + 1;
         pragma Debug (O ("number of  tasks initialized : "
                          & Integer 'Image (Current)));
      end Initialize_Id;

      -------------------------
      -- Pool_Manager.Lookup --
      -------------------------

      procedure Lookup (Tid : Ada.Task_Identification.Task_Id;
                        Result : out Integer) is
         J : Integer := Thread_Index_Type'First;
         use Ada.Task_Identification;
         --  Result : Ravenscar_Thread_Access;
      begin
         pragma Assert (Package_Initialized);
         while My_Task_Id_Arr (J) /= Tid loop
            pragma Assert (J /= Thread_Index_Type'Last);
            J := J + 1;
         end loop;
         Result := J;
      end Lookup;

      --------------------------------------------------
      -- Pool_Manager.Wait_For_Package_Initialization --
      --------------------------------------------------

      entry Wait_For_Package_Initialization when Package_Initialized is
      begin
         null;
      end Wait_For_Package_Initialization;

   end Pool_Manager;

   -----------------
   -- Run_In_Task --
   -----------------

   function Run_In_Task
     (TF               : access Ravenscar_Thread_Factory_Type;
      Name             : String := "";
      Default_Priority : System.Any_Priority := System.Default_Priority;
      P                : Parameterless_Procedure)
     return Thread_Access is
      pragma Warnings (Off);
      pragma Unreferenced (TF);
      pragma Unreferenced (Name);
      pragma Unreferenced (Default_Priority);
      pragma Warnings (On);
      --  XXX The use of names and priorities is not implemented yet.
      Id : Thread_Index_Type;
      T  : Thread_Access;
   begin
      pragma Assert (Initialized);
      --  The following call should not be executed in a protected
      --  object, because it can be blocking.
      Thread_Index_Manager.Get (Id);
      Pool_Manager.Create_Thread (Id, P, T);
      declare
         RT : constant Ravenscar_Thread_Access
           := Ravenscar_Thread_Access (T);
      begin
         pragma Assert (RT.Id.Id /= Main_Task_Id);
         pragma Debug (O ("launch task "
                          & Integer'Image (RT.Id.Id)
                          &" waiting on "
                          & Integer'Image (Integer (RT.Id.Sync_Id))));
         --  Sync_Pool (RT.Id.Sync_Id).Signal;
         Resume (RT.Id.Sync_Id);
         return T;
      end;
   end Run_In_Task;

   function Run_In_Task
     (TF               : access Ravenscar_Thread_Factory_Type;
      Name             : String := "";
      Default_Priority : System.Any_Priority := System.Default_Priority;
      R                : Runnable_Access;
      C                : Runnable_Controller_Access)
     return Thread_Access is
      pragma Warnings (Off);
      pragma Unreferenced (TF);
      pragma Unreferenced (Name);
      pragma Unreferenced (Default_Priority);
      pragma Warnings (On);
      --  XXX The use of names and priorities is not implemented yet.
      Id : Thread_Index_Type;
      T  : Thread_Access;
   begin
      pragma Assert (Initialized);
      --  The following call should not be executed in a protected
      --  object, because it can be blocking.
      Thread_Index_Manager.Get (Id);
      Pool_Manager.Create_Thread (Id, R, C, T);
      declare
         RT : constant Ravenscar_Thread_Access
           := Ravenscar_Thread_Access (T);
      begin
         pragma Assert (RT.Id.Id /= Main_Task_Id);
         pragma Debug (O ("launch task "
                          & Integer'Image (RT.Id.Id)
                          &" waiting on "
                          & Integer'Image (Integer (RT.Id.Sync_Id))));
         --  Sync_Pool (RT.Id.Sync_Id).Signal;
         Resume (RT.Id.Sync_Id);
         return T;
      end;
   end Run_In_Task;

   ------------------
   -- Set_Priority --
   ------------------

   procedure Set_Priority
     (TF : access Ravenscar_Thread_Factory_Type;
      T  : Thread_Id'Class;
      P  : System.Any_Priority) is
      pragma Warnings (Off);
      pragma Unreferenced (TF);
      pragma Unreferenced (T);
      pragma Unreferenced (P);
      pragma Warnings (On);
   begin
      raise Tasking.Tasking_Profile_Error;
   end Set_Priority;

   -----------------
   -- Simple_Task --
   -----------------

   task body Simple_Task is
      Id    : Integer;
      Tid   : constant Ada.Task_Identification.Task_Id
        := Ada.Task_Identification.Current_Task;
      --  Th_Id : Ravenscar_Thread_Id;
   begin
      Pool_Manager.Initialize_Id (Tid, Id);
      Synchro_Index_Manager.Initialize (False);
      My_Thread_Arr (Id).Id.Id := Id;
      My_Thread_Arr (Id).Id.Sync_Id := Prepare_Suspend;
      Pool_Manager.End_Initialization (Id);
      loop
         Suspend (My_Thread_Arr (Id).Id.Sync_Id);
         if My_Job_Passing_Arr (Id) = Use_Runnable then
            Run (My_Runnable_Arr (Id));
         else
            My_PP_Arr (Id).all;
         end if;
         pragma Assert (My_Controller_Arr (Id) /= null);
         Free_Runnable (My_Controller_Arr (Id).all, My_Runnable_Arr (Id));
         --  Free (My_Controller_Arr (Id));
         My_Thread_Arr (Id).Id.Sync_Id := Prepare_Suspend;
         Thread_Index_Manager.Release (Id);
      end loop;
   end Simple_Task;

   ------------
   -- Resume --
   ------------

   procedure Resume (S : Synchro_Index_Type) is
   begin
      pragma Assert (Initialized);
      pragma Debug (O ("Resume on " & Integer'Image (Integer (S))));
      Sync_Pool (S).Signal;
   end Resume;

   -------------
   -- Suspend --
   -------------

   procedure Suspend (S : Synchro_Index_Type) is
   begin
      pragma Debug (O ("will suspend: " & Integer'Image (Integer (S))));
      Sync_Pool (S).Wait;
      pragma Assert (not Sync_Pool (S).Get_Signaled and
                     not Sync_Pool (S).Get_Waiting);
      --  XXX might fail because of a bug in GNAT 3.15a1 ...
      --  The call to wait didn't work.

      pragma Debug (O ("end suspend: " & Integer'Image (Integer (S))));

      Synchro_Index_Manager.Release (Synchro_Index_Manager.Index_Type (S));
   end Suspend;

   use PolyORB.Initialization;
   use PolyORB.Initialization.String_Lists;
   use PolyORB.Utils.Strings;

begin
   Register_Module
     (Module_Info'
      (Name => +"tasking.profiles.ravenscar.threads",
       Conflicts => Empty,
       Depends => Empty,
       Provides => +"tasking.threads",
       Init => Initialize'Access));
end PolyORB.Tasking.Profiles.Ravenscar.Threads;
