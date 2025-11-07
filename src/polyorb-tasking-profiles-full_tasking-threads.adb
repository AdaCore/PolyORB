------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--              POLYORB.TASKING.PROFILES.FULL_TASKING.THREADS               --
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

--  Implementation of Threads under the Full_Tasking profile.

--  WAG:601
--  pragma Warnings (Off) with pattern not supported in that compiler version
--  so use plain pragma Warnings (Off/On) instead.
--  pragma Warnings (Off, "* is an internal GNAT unit");
--  pragma Warnings (Off, "use of this unit is non-portable*");

pragma Warnings (Off);
--  Depends on System.Tasking.Utilities, an internal GNAT unit
with System.Tasking.Utilities;
pragma Warnings (On);

with Ada.Exceptions;
with Ada.Real_Time;
with PolyORB.Utils.Unchecked_Deallocation;
with Ada.Unchecked_Conversion;

with PolyORB.Initialization;
with PolyORB.Log;
with PolyORB.Parameters;
with PolyORB.Platform;
with PolyORB.Utils.Strings;

package body PolyORB.Tasking.Profiles.Full_Tasking.Threads is

   use PolyORB.Log;

   package L is new PolyORB.Log.Facility_Log
     ("polyorb.tasking.profiles.full_tasking.threads");
   procedure O (Message : String; Level : Log_Level := Debug) renames L.Output;
   function C (Level : Log_Level := Debug) return Boolean renames L.Enabled;

   --  Task type

   task type Generic_Task
     (P : System.Priority; S : Natural; N : access String)
   is
      --  All purpose generic task that executes a 'Runnable'

      pragma Priority (P);
      pragma Storage_Size (S);
      pragma Task_Name ("GT-" & N.all);

      entry Initialize (T : PTT.Thread_Access);
      --  Initialize the task

      entry Start (Run : PTT.Runnable_Access);
      --  Start the task

   end Generic_Task;

   type Generic_Task_Access is access Generic_Task;
   procedure Free_Generic_Task is new PolyORB.Utils.Unchecked_Deallocation.Free
     (Object => Generic_Task,
      Name => Generic_Task_Access);

   type Full_Tasking_Thread_Type is new PTT.Thread_Type with record
      Id         : PTT.Thread_Id;
      Self       : Generic_Task_Access;
      Priority   : System.Any_Priority;
      Stack_Size : Natural;
   end record;

   overriding function Get_Thread_Id
     (T : access Full_Tasking_Thread_Type) return PTT.Thread_Id;

   type Full_Tasking_Thread_Access
      is access all Full_Tasking_Thread_Type'Class;

   procedure Free is new PolyORB.Utils.Unchecked_Deallocation.Free
     (Object => Full_Tasking_Thread_Type'Class,
      Name => Full_Tasking_Thread_Access);

   function A_To_P_Task_Id (ATID : Ada.Task_Identification.Task_Id)
     return PTT.Thread_Id;
   pragma Inline (A_To_P_Task_Id);
   --  Convert Ada Task_Id to PolyORB Task_Id.

   type Simple_Runnable is new PTT.Runnable with record
      Main_Subprogram : PTT.Parameterless_Procedure;
   end record;
   --  Simplified runnable for parameterless procedure

   overriding procedure Run (SR : not null access Simple_Runnable);

   --  WAG:642
   --  For older compilers, we need a separate Reaper task to purge terminated
   --  generic tasks. (In newer versions, we can just mark them to be freed
   --  automatically upon termination, see NF-65-H911-007).

   task type Reaper is
      entry Free (GT : Generic_Task_Access);
      --  Busy-wait for the designated task to terminate, then free it
   end Reaper;

   ------------
   -- Reaper --
   ------------

   task body Reaper is
      Terminated_Task : Generic_Task_Access;
   begin
      loop
         select
            accept Free (GT : Generic_Task_Access) do
               Terminated_Task := GT;
            end Free;

            for Tries in 1 .. 3 loop
               if Terminated_Task'Terminated then
                  pragma Debug (C, O ("Reaper: freeing generic task "
                    & Ada.Task_Identification.Image
                        (Terminated_Task'Identity)));
                  Free_Generic_Task (Terminated_Task);
                  exit;
               end if;
               delay 0.1;
            end loop;

            if Terminated_Task /= null then
               O ("Reaper: giving up on non-terminating task "
                 & Ada.Task_Identification.Image (Terminated_Task'Identity),
                  Notice);
            end if;
         or
            terminate;
         end select;
      end loop;
   end Reaper;

   The_Reaper : access Reaper;

   --------------------
   -- P_To_A_Task_Id --
   --------------------

   function P_To_A_Task_Id (TID : PTT.Thread_Id)
     return Ada.Task_Identification.Task_Id
   is
      function STID_To_ATID is new Ada.Unchecked_Conversion
        (System.Tasking.Task_Id, Ada.Task_Identification.Task_Id);
   begin
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

   ---------
   -- Run --
   ---------

   overriding procedure Run (SR : not null access Simple_Runnable)
   is
      use type PTT.Parameterless_Procedure;
   begin
      if SR.Main_Subprogram /= null then
         SR.Main_Subprogram.all;
      end if;
   end Run;

   -----------------
   -- Run_In_Task --
   -----------------

   overriding function Run_In_Task
     (TF               : access Full_Tasking_Thread_Factory_Type;
      Name             : String;
      Default_Priority : System.Any_Priority := System.Default_Priority;
      Storage_Size     : Natural := 0;
      R                : PTT.Runnable_Access) return PTT.Thread_Access
   is
      pragma Warnings (Off);
      pragma Unreferenced (TF);
      pragma Warnings (On);

      T  : constant Full_Tasking_Thread_Access := new Full_Tasking_Thread_Type;
      GT : Generic_Task_Access;

   begin
      T.Priority :=
        System.Priority
          (Parameters.Get_Conf
            ("tasking",
             "polyorb.tasking.threads." & Name & ".priority",
             Default_Priority));

      if Storage_Size = 0 then
         T.Stack_Size := Parameters.Get_Conf
                           ("tasking",
                            "storage_size",
                            PTT.Default_Storage_Size);
      else
         T.Stack_Size := Storage_Size;
      end if;

      GT := new Generic_Task
                  (T.Priority, T.Stack_Size, Name'Unrestricted_Access);
      T.Self := GT;
      GT.Initialize (PTT.Thread_Access (T));
      GT.Start (R);
      return PTT.Thread_Access (T);
   end Run_In_Task;

   overriding function Run_In_Task
     (TF               : access Full_Tasking_Thread_Factory_Type;
      Name             : String;
      Default_Priority : System.Any_Priority := System.Default_Priority;
      Storage_Size     : Natural := 0;
      P                : PTT.Parameterless_Procedure) return PTT.Thread_Access
   is
      R : constant PTT.Runnable_Access := new Simple_Runnable;

   begin
      Simple_Runnable (R.all).Main_Subprogram := P;

      return Run_In_Task
        (TF,
         Name,
         Default_Priority,
         Storage_Size,
         R);
   end Run_In_Task;

   ------------------
   -- Generic_Task --
   ------------------

   task body Generic_Task is
      The_Thread     : Full_Tasking_Thread_Access;
      The_Runnable   : PTT.Runnable_Access;
      Self           : Generic_Task_Access;

   begin
      accept Initialize (T : PTT.Thread_Access) do
         The_Thread := Full_Tasking_Thread_Access (T);
         The_Thread.Id := A_To_P_Task_Id
           (Ada.Task_Identification.Current_Task);
         --  XXX Maybe The_Thread.Id could be suppressed altogether!!
         Self := The_Thread.Self;
      end Initialize;

      accept Start (Run : PTT.Runnable_Access) do
         The_Runnable := Run;
      end Start;

      begin
         PTT.Run (The_Runnable);
      exception
         when E : others =>
            pragma Debug (C, O ("Generic_Task: " & PTT.Image (The_Thread.Id)
              & " abnormal termination: "
              & Ada.Exceptions.Exception_Information (E)));
            null;
      end;

      PTT.Free (The_Runnable);

      --  Generic task is about to terminate: after this point, the value of
      --  The_Thread that Run_In_Task returned to the caller is becoming
      --  invalid. Hopefully we've discarded it by then.

      Free (The_Thread);

      --  Here we signal the GNAT runtime that it can forget this task
      --  altogether (and deallocate its ATCB when it terminates). For older
      --  runtime versions that do not support this feature, we use a separate
      --  reaper task for this purpose.

      if Platform.Free_On_Termination then

         --  Note: It is a bounded error to call Unchecked_Deallocation on
         --  Self because the task has discriminants (13.11.2(11)). However
         --  no problem in practice as we are not going to reference them
         --  beyond this point.

         Free_Generic_Task (Self);
      else
         The_Reaper.Free (Self);
      end if;

   end Generic_Task;

   ---------------------------
   -- Get_Current_Thread_Id --
   ---------------------------

   overriding function Get_Current_Thread_Id
     (TF : access Full_Tasking_Thread_Factory_Type) return PTT.Thread_Id
   is
      pragma Warnings (Off);
      pragma Unreferenced (TF);
      pragma Warnings (On);
   begin
      return A_To_P_Task_Id (Ada.Task_Identification.Current_Task);
   end Get_Current_Thread_Id;

   -------------------
   -- Get_Thread_Id --
   -------------------

   overriding function Get_Thread_Id
     (T : access Full_Tasking_Thread_Type) return PTT.Thread_Id is
   begin
      return T.Id;
   end Get_Thread_Id;

   -----------
   -- Image --
   -----------

   overriding function Thread_Id_Image
     (TF  : access Full_Tasking_Thread_Factory_Type;
      TID : PTT.Thread_Id) return String
   is
      use PTT;

      pragma Warnings (Off);
      pragma Unreferenced (TF);
      pragma Warnings (On);
   begin
      if TID = Null_Thread_Id then
         return "<null thread id>";
      else
         return Ada.Task_Identification.Image (P_To_A_Task_Id (TID));
      end if;
   end Thread_Id_Image;

   ------------------
   -- Set_Priority --
   ------------------

   overriding procedure Set_Priority
     (TF : access Full_Tasking_Thread_Factory_Type;
      T  :        PTT.Thread_Id;
      P  :        System.Any_Priority)
   is
   begin
      Set_Priority_P.all (TF, T, P);
   end Set_Priority;

   ------------------
   -- Get_Priority --
   ------------------

   overriding function Get_Priority
     (TF : access Full_Tasking_Thread_Factory_Type;
      T  :        PTT.Thread_Id)
     return System.Any_Priority
   is
   begin
      return Get_Priority_P.all (TF, T);
   end Get_Priority;

   --------------------
   -- Relative_Delay --
   --------------------

   overriding procedure Relative_Delay
     (TF : access Full_Tasking_Thread_Factory_Type; D : Duration)
   is
      pragma Unreferenced (TF);
   begin
      delay D;
   end Relative_Delay;

   -----------------
   -- Awake_Count --
   -----------------

   overriding function Awake_Count
     (TF : access Full_Tasking_Thread_Factory_Type) return Natural is
   begin
      --  If the environment task is not callable, we do not count it as awake

      if TF.Environment_Task.Callable then
         return TF.Environment_Task.Awake_Count;
      else
         return TF.Environment_Task.Awake_Count - 1;
      end if;
   end Awake_Count;

   -----------------------
   -- Independent_Count --
   -----------------------

   overriding function Independent_Count
     (TF : access Full_Tasking_Thread_Factory_Type) return Natural
   is
      pragma Unreferenced (TF);
   begin
      return System.Tasking.Utilities.Independent_Task_Count;
   end Independent_Count;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize;

   procedure Initialize is
      use Ada.Real_Time;
      use System.Tasking;

      Time_0 : constant Time := Time_Of (0, Time_Span_Zero);

      S_TID : System.Tasking.Task_Id;
      A_TID : Ada.Task_Identification.Task_Id;
      for A_TID'Address use S_TID'Address;
      pragma Import (Ada, A_TID);
      --  Task identifier used to climb up task tree until we reach the
      --  environment task.

   begin
      PTT.Node_Boot_Time := To_Duration (Clock - Time_0);
      PTT.Register_Thread_Factory (PTT.Thread_Factory_Access
                                   (The_Thread_Factory));
      S_TID := System.Tasking.Self;
      while S_TID.Common.Parent /= null loop
         S_TID := S_TID.Common.Parent;
      end loop;
      The_Thread_Factory.Environment_Task := S_TID;

      if not Platform.Free_On_Termination then
         The_Reaper := new Reaper;
         pragma Debug
           (C, O ("Reaper task started: "
                  & Ada.Task_Identification.Image (The_Reaper'Identity)));
      end if;

      pragma Debug (C, O ("Environment task: "
                            & Ada.Task_Identification.Image (A_TID)));
   end Initialize;

   use PolyORB.Initialization;
   use PolyORB.Initialization.String_Lists;
   use PolyORB.Utils.Strings;

begin
   Register_Module
     (Module_Info'
      (Name      => +"tasking.profiles.full_tasking.threads",
       Conflicts => Empty,
       Depends   => +"full_tasking.threads.priorities",
       Provides  => +"tasking.threads",
       Implicit  => False,
       Init      => Initialize'Access,
       Shutdown  => null));
end PolyORB.Tasking.Profiles.Full_Tasking.Threads;
