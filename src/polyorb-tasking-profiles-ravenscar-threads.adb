------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--               POLYORB.TASKING.PROFILES.RAVENSCAR.THREADS                 --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2002-2008, Free Software Foundation, Inc.          --
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

--  Implementation of Threads under the Ravenscar profile.

--  WAG:601
--  pragma Warnings (Off) with pattern not supported in that compiler version
--  so use plain pragma Warnings (Off/On) instead.
--  pragma Warnings (Off, "* is an internal GNAT unit");
--  pragma Warnings (Off, "use of this unit is non-portable*");

pragma Warnings (Off);
--  Depends on System.Tasking.Utilities, an internal GNAT unit
with System.Tasking.Utilities;
pragma Warnings (On);

with Ada.Real_Time;
with Ada.Task_Identification;
with Ada.Unchecked_Conversion;

with PolyORB.Log;
with PolyORB.Utils.Strings;

package body PolyORB.Tasking.Profiles.Ravenscar.Threads is

   use PolyORB.Log;

   package L is new PolyORB.Log.Facility_Log
     ("polyorb.tasking.profiles.ravenscar.threads");
   procedure O (Message : String; Level : Log_Level := Debug)
     renames L.Output;
   function C (Level : Log_Level := Debug) return Boolean
     renames L.Enabled;

   ---------
   -- Ids --
   ---------

   --  Id are associated to the tasks created from this package.
   --  The last Id is reserved to the main context, which
   --  is the task that execute the Initialize procedure of this package.
   --  In this package, it is called the main task, and the Thread object
   --  associated to it is called the main Thread.

   package Thread_Index_Manager is
      new PolyORB.Tasking.Profiles.Ravenscar.Index_Manager
     (Number_Of_System_Tasks - 1);

   subtype Task_Index_Type is Thread_Index_Manager.Index_Type;
   --  Type of the Ids of the Threads that are not the one of the main task.

   subtype Thread_Index_Type is Integer
     range Task_Index_Type'First .. Task_Index_Type'Last + 1;
   --  Type of the Ids of all the Threads, including the one
   --  of the main task

   --  Paramaters associated to this main task :

   Main_Task_Index : constant Integer := Thread_Index_Type'Last;
   Main_Task_Tid   : Ada.Task_Identification.Task_Id;

   --  XXX These two functions are duplicated from Full_Tasking

   function P_To_A_Task_Id (TID : PTT.Thread_Id)
     return Ada.Task_Identification.Task_Id;
   pragma Inline (P_To_A_Task_Id);
   --  Convert PolyORB Task_Id to Ada Task_Id

   function A_To_P_Task_Id (ATID : Ada.Task_Identification.Task_Id)
     return PTT.Thread_Id;
   pragma Inline (A_To_P_Task_Id);
   --  Convert Ada Task_Id to PolyORB Task_Id

   --------------------
   -- P_To_A_Task_Id --
   --------------------

   function P_To_A_Task_Id (TID : PTT.Thread_Id)
     return Ada.Task_Identification.Task_Id
   is
      function STID_To_ATID is new Ada.Unchecked_Conversion
        (System.Tasking.Task_Id, Ada.Task_Identification.Task_Id);
   begin
      --  Casing of To_Task_ID has changed.
      return STID_To_ATID (System.Tasking.To_Task_Id (PTT.To_Address (TID)));
   end P_To_A_Task_Id;

   --------------------
   -- A_To_P_Task_Id --
   --------------------

   function A_To_P_Task_Id (ATID : Ada.Task_Identification.Task_Id)
     return PTT.Thread_Id
   is
      function ATID_To_STID is new Ada.Unchecked_Conversion
        (Ada.Task_Identification.Task_Id, System.Tasking.Task_Id);
   begin
      return PTT.To_Thread_Id
        (System.Tasking.To_Address (ATID_To_STID (ATID)));
   end A_To_P_Task_Id;

   -------------------
   -- Tasking Types --
   -------------------

   --  Tasking type used in the pools preallocated by this package:

   task type Simple_Task is
      pragma Storage_Size (Storage_Size);
      pragma Priority (Task_Priority);
   end Simple_Task;
   --  Type of the task that run the submitted threads

   protected type Barrier is
      --  Type of the internal synchronisation object of the tasks
      --  allocated through this package.
      --  A call to Suspend will result in a call to Wait;
      --  a call to Resume will result in a call to Signal.

      procedure Prepare_Wait;
      --  Initialize the barrier for a call to Wait. If it is already
      --  prepared to Wait, raise an assertion failure.

      procedure Abort_Wait;
      --  Abort the previous call to Prepare_Wait. If no call to Prepare_Wait
      --  has been done, raise an assertion failure.

      entry Wait;
      --  Wait until it is signaled

      procedure Signal;
      --  Signal the Suspension_Object

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
   --  Table of the Task_Id of the task of the pool

   protected Pool_Manager is
      --  Protected manager for the pool

      procedure Initialize_Id
        (Tid : Ada.Task_Identification.Task_Id;
         Idx : out Thread_Index_Type);
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
      --  This is the protected section of the Create_Thread procedure

      function Lookup (Tid : Ada.Task_Identification.Task_Id)
                      return Integer;
      --  Get the Thread_Access associated to the given Task_Id

      procedure Initialize;
      --  Initialisation procedure of Pool_Manager

   private
      Package_Initialized : Boolean := False;
      Current             : Integer := Task_Index_Type'First;
      My_Task_Id_Arr      : Task_Id_Arr;
   end Pool_Manager;

   type Thread_Arr is array (Thread_Index_Type)
     of aliased Ravenscar_Thread_Type;

   My_Thread_Arr  : Thread_Arr;
   --  Pool of Threads

   type Runnable_Arr is array (Thread_Index_Type)
     of Runnable_Access;

   type Controller_Arr is array (Thread_Index_Type)
     of Runnable_Controller_Access;

   type Job_Passing is (Use_Runnable, Use_PP);
   --  There is two way to pass a job to the tasks:
   --  by a Runnable or by a Parameterless_Procedure.
   --  This type is used to discriminate.

   type Job_Passing_Arr is array (Thread_Index_Type) of Job_Passing;

   My_Job_Passing_Arr : Job_Passing_Arr;

   My_Runnable_Arr : Runnable_Arr;
   --  Pool of Runnables

   My_Controller_Arr : Controller_Arr;
   --  Pool of Runnable_Controllers

   type PP_Arr is array (Thread_Index_Type)
     of Parameterless_Procedure;

   My_PP_Arr : PP_Arr;
   --  Pool of Parameterless_Procedure

   Task_Pool : array (Task_Index_Type) of Simple_Task;
   pragma Warnings (Off);
   pragma Unreferenced (Task_Pool);
   pragma Warnings (On);
   --  Pool of preallocated tasks

   type Barrier_Arr is array (Synchro_Index_Type)
     of Barrier;

   Sync_Pool : Barrier_Arr;
   --  Pool of Barrier used for synchronisations

   -------------------
   -- Abort_Suspend --
   -------------------

   procedure Abort_Suspend
     (S : Synchro_Index_Type) is
   begin
      pragma Debug (C, O ("abort suspend on " & Integer'Image (Integer (S))));

      Sync_Pool (S).Abort_Wait;

      pragma Debug (C, O ("abort done on " & Integer'Image (Integer (S))));

      Synchro_Index_Manager.Release (Synchro_Index_Manager.Index_Type (S));
   end Abort_Suspend;

   -------------
   -- Barrier --
   -------------

   protected body Barrier is

      -------------------------
      -- Barrier.Get_Waiting --
      -------------------------

      function Get_Waiting return Boolean is
      begin
         return Waiting;
      end Get_Waiting;

      --------------------------
      -- Barrier.Get_Signaled --
      --------------------------

      function Get_Signaled return Boolean is
      begin
         return Signaled;
      end Get_Signaled;

      --------------------------
      -- Barrier.Prepare_Wait --
      --------------------------

      procedure Prepare_Wait is
      begin
         pragma Assert (not Signaled);
         --  Why should we be signaled if we are not waiting yet?
         --  It would definitely be an error.

         pragma Assert (not Waiting);
         --  Fail if it is the second call to Prepare_Wait

         Waiting := True;
      end Prepare_Wait;

      ------------------------
      -- Barrier.Abort_Wait --
      ------------------------

      procedure Abort_Wait is
      begin
         pragma Assert (Waiting);
         --  Fail if we try to abort, but no call to suspend were prepared.

         Waiting := False;
      end Abort_Wait;

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
         pragma Debug (C, O ("wait done!"));

         Signaled := False;
         Waiting := False;
      end Wait;

   end Barrier;

   ---------------------------
   -- Get_Current_Thread_Id --
   ---------------------------

   function Get_Current_Thread_Id
     (TF : access Ravenscar_Thread_Factory_Type)
     return Thread_Id
   is
      pragma Warnings (Off);
      pragma Unreferenced (TF);
      pragma Warnings (On);

   begin
      return A_To_P_Task_Id (Ada.Task_Identification.Current_Task);
   end Get_Current_Thread_Id;

   ---------------------
   -- Thread_Id_Image --
   ---------------------

   function Thread_Id_Image
     (TF  : access Ravenscar_Thread_Factory_Type;
      TID : PTT.Thread_Id)
     return String
   is
      pragma Warnings (Off);
      pragma Unreferenced (TF);
      pragma Warnings (On);

      Index_Image : constant String := Integer'Image (Get_Thread_Index (TID));
   begin
      return Ada.Task_Identification.Image (P_To_A_Task_Id (TID))
        & "(" & Index_Image (Index_Image'First + 1 .. Index_Image'Last) & ")";
   end Thread_Id_Image;

   -------------------
   -- Get_Thread_Id --
   -------------------

   function Get_Thread_Id
     (T : access Ravenscar_Thread_Type)
      return Thread_Id is
   begin
      return T.Id;
   end Get_Thread_Id;

   ------------------
   -- Set_Priority --
   ------------------

   procedure Set_Priority
     (TF : access Ravenscar_Thread_Factory_Type;
      T  :        PTT.Thread_Id;
      P  :        System.Any_Priority)
   is
      pragma Warnings (Off);
      pragma Unreferenced (TF);
      pragma Unreferenced (T);
      pragma Unreferenced (P);
      pragma Warnings (On);
   begin
      raise Tasking_Error;
   end Set_Priority;

   ------------------
   -- Get_Priority --
   ------------------

   function Get_Priority
     (TF : access Ravenscar_Thread_Factory_Type;
      T  :        PTT.Thread_Id)
     return System.Any_Priority
   is
      pragma Warnings (Off);
      pragma Unreferenced (TF);
      pragma Warnings (On);

      Index : constant Integer := Get_Thread_Index (T);
      pragma Unreferenced (Index);
      --  Note: we compute index only to check T belongs to system
      --  tasks. If not, getting its priority is meaningless.

   begin
      return Task_Priority;
   end Get_Priority;

   ----------------------
   -- Get_Thread_Index --
   ----------------------

   function Get_Thread_Index (T : Thread_Id) return Integer is
   begin
      return Pool_Manager.Lookup (P_To_A_Task_Id (T));
   end Get_Thread_Index;

   -------------
   -- Stopper --
   -------------

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
      pragma Debug (C, O ("prepare suspend on" & S'Img));
      Sync_Pool (S).Prepare_Wait;
      pragma Debug (C, O ("prepared susped on" & S'Img));
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
         T   : out Thread_Access)
      is
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
         T  : out Thread_Access)
      is
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

      procedure End_Initialization
        (Id : Thread_Index_Type)
      is
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
         My_Task_Id_Arr (Main_Task_Index) := Main_Task_Tid;
         My_Thread_Arr (Main_Task_Index).Id := A_To_P_Task_Id (Main_Task_Tid);
      end Initialize;

      --------------------------------
      -- Pool_Manager.Initialize_Id --
      --------------------------------

      procedure Initialize_Id
        (Tid : Ada.Task_Identification.Task_Id;
         Idx : out Thread_Index_Type) is
      begin
         pragma Assert (not Package_Initialized);
         pragma Assert (Current <= Thread_Index_Type'Last);

         Idx := Current;
         My_Thread_Arr (Current).Id := A_To_P_Task_Id (Tid);
         My_Task_Id_Arr (Current) := Tid;
         Current := Current + 1;
         pragma Debug (C, O ("number of tasks initialized : "
                          & Integer'Image (Current)));
      end Initialize_Id;

      -------------------------
      -- Pool_Manager.Lookup --
      -------------------------

      function Lookup (Tid : Ada.Task_Identification.Task_Id)
                      return Integer
      is
         J : Integer := Thread_Index_Type'First;
         use Ada.Task_Identification;

      begin
         pragma Assert (Package_Initialized);

         while My_Task_Id_Arr (J) /= Tid loop
            if J = Thread_Index_Type'Last then
               --  Tis is not managed by this pool

               raise Tasking_Error;
            end if;

            J := J + 1;
         end loop;
         return J;
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

   --  XXX TOO MUCH CODE DUPLICATION!
   --  Should be a simple call to the Runnable version!

   function Run_In_Task
     (TF               : access Ravenscar_Thread_Factory_Type;
      Name             : String := "";
      Default_Priority : System.Any_Priority := System.Default_Priority;
      Storage_Size     : Natural := 0;
      P                : Parameterless_Procedure)
     return Thread_Access
   is
      pragma Warnings (Off);
      pragma Unreferenced (TF);
      pragma Unreferenced (Name);
      pragma Warnings (On);

      --  XXX The use of names is not implemented yet.
      Id : Thread_Index_Type;
      T  : Thread_Access;
   begin
      if Default_Priority /= Task_Priority
        or else (Storage_Size /= 0
                 and then Storage_Size /=
                 Tasking.Profiles.Ravenscar.Threads.Storage_Size)
      then
         raise Tasking_Error;
      end if;

      --  The following call should not be executed in a protected
      --  object, because it can be blocking.
      Thread_Index_Manager.Get (Id);
      Pool_Manager.Create_Thread (Id, P, T);

      declare
         RT : constant Ravenscar_Thread_Access
           := Ravenscar_Thread_Access (T);
      begin
         pragma Assert (Get_Thread_Index (RT.Id) /= Main_Task_Index);
         pragma Debug (C, O ("launch task "
                          & Image (RT.Id)
                          &" waiting on "
                          & Integer'Image (Integer (RT.Sync_Id))));
         --  Sync_Pool (RT.Id.Sync_Id).Signal;
         Resume (RT.Sync_Id);
         return T;
      end;
   end Run_In_Task;

   function Run_In_Task
     (TF               : access Ravenscar_Thread_Factory_Type;
      Name             : String := "";
      Default_Priority : System.Any_Priority := System.Default_Priority;
      Storage_Size     : Natural := 0;
      R                : Runnable_Access;
      RC               : Runnable_Controller_Access) return Thread_Access
   is
      pragma Warnings (Off);
      pragma Unreferenced (TF);
      pragma Unreferenced (Name);
      pragma Warnings (On);

      --  XXX The use of names is not implemented yet.
      Id : Thread_Index_Type;
      T  : Thread_Access;
   begin
      if Default_Priority /= Task_Priority
        or else (Storage_Size /= 0
                 and then Storage_Size /=
                 Tasking.Profiles.Ravenscar.Threads.Storage_Size)
      then
         raise Tasking_Error;
      end if;

      --  The following call should not be executed in a protected
      --  object, because it can be blocking.
      Thread_Index_Manager.Get (Id);
      Pool_Manager.Create_Thread (Id, R, RC, T);

      declare
         RT : constant Ravenscar_Thread_Access
           := Ravenscar_Thread_Access (T);
      begin
         pragma Assert (Get_Thread_Index (RT.Id) /= Main_Task_Index);
         pragma Debug (C, O ("launch task "
                          & Image (RT.Id)
                          &" waiting on "
                          & Integer'Image (Integer (RT.Sync_Id))));
         --  Sync_Pool (RT.Id.Sync_Id).Signal;
         Resume (RT.Sync_Id);
         return T;
      end;
   end Run_In_Task;

   -----------------
   -- Simple_Task --
   -----------------

   task body Simple_Task is
      Index : Integer;
      Tid   : constant Ada.Task_Identification.Task_Id
        := Ada.Task_Identification.Current_Task;

   begin
      Pool_Manager.Initialize_Id (Tid, Index);
      Synchro_Index_Manager.Initialize (False);
      My_Thread_Arr (Index).Sync_Id := Prepare_Suspend;
      Pool_Manager.End_Initialization (Index);
      loop
         Suspend (My_Thread_Arr (Index).Sync_Id);
         if My_Job_Passing_Arr (Index) = Use_Runnable then
            Run (My_Runnable_Arr (Index));
         else
            My_PP_Arr (Index).all;
         end if;
         pragma Assert (My_Controller_Arr (Index) /= null);
         Free_Runnable
           (My_Controller_Arr (Index).all, My_Runnable_Arr (Index));
         --  Free (My_Controller_Arr (Index));
         My_Thread_Arr (Index).Sync_Id := Prepare_Suspend;
         Thread_Index_Manager.Release (Index);
      end loop;
   end Simple_Task;

   ------------
   -- Resume --
   ------------

   procedure Resume (S : Synchro_Index_Type) is
   begin
      pragma Debug (C, O ("Resume on " & Integer'Image (Integer (S))));
      Sync_Pool (S).Signal;
   end Resume;

   -------------
   -- Suspend --
   -------------

   procedure Suspend (S : Synchro_Index_Type) is
   begin
      pragma Debug (C, O ("will suspend: " & Integer'Image (Integer (S))));

      Sync_Pool (S).Wait;

      pragma Assert (not Sync_Pool (S).Get_Signaled and
                     not Sync_Pool (S).Get_Waiting);
      --  XXX might fail because of a bug in GNAT 3.15a1 ...
      --  The call to wait didn't work.

      pragma Debug (C, O ("end suspend: " & Integer'Image (Integer (S))));

      Synchro_Index_Manager.Release (Synchro_Index_Manager.Index_Type (S));
   end Suspend;

   --------------------
   -- Relative_Delay --
   --------------------

   procedure Relative_Delay
     (TF : access Ravenscar_Thread_Factory_Type; D : Duration)
   is
      pragma Unreferenced (TF);

      use Ada.Real_Time;

      Deadline : constant Time := Clock + To_Time_Span (D);
   begin
      delay until Deadline;
   end Relative_Delay;

   -----------------
   -- Awake_Count --
   -----------------

   function Awake_Count (TF : access Ravenscar_Thread_Factory_Type)
     return Natural
   is
   begin

      --  If the environment task is not callable we do not count it as awake

      if TF.Environment_Task.Callable then
         return TF.Environment_Task.Awake_Count;
      else
         return TF.Environment_Task.Awake_Count - 1;
      end if;
   end Awake_Count;

   -----------------------
   -- Independent_Count --
   -----------------------

   function Independent_Count (TF : access Ravenscar_Thread_Factory_Type)
     return Natural
   is
      pragma Unreferenced (TF);
   begin
      return System.Tasking.Utilities.Independent_Task_Count;
   end Independent_Count;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is
      use Ada.Real_Time;
      Time_0 : constant Time := Time_Of (0, Time_Span_Zero);
   begin
      PTT.Node_Boot_Time := To_Duration (Clock - Time_0);
      Thread_Index_Manager.Initialize;
      Synchro_Index_Manager.Initialize (False);
      Main_Task_Tid := Ada.Task_Identification.Current_Task;
      The_Thread_Factory.Environment_Task := System.Tasking.Self;
      Pool_Manager.Initialize;
      PTT.Register_Thread_Factory (PTT.Thread_Factory_Access
                                   (The_Thread_Factory));
      Pool_Manager.Wait_For_Package_Initialization;
   end Initialize;

   use PolyORB.Initialization;
   use PolyORB.Initialization.String_Lists;
   use PolyORB.Utils.Strings;

begin
   Register_Module
     (Module_Info'
      (Name      => +"tasking.profiles.ravenscar.threads",
       Conflicts => Empty,
       Depends   => Empty,
       Provides  => +"tasking.threads",
       Implicit  => False,
       Init      => Initializer,
       Shutdown  => null));
end PolyORB.Tasking.Profiles.Ravenscar.Threads;
