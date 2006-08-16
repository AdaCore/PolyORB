------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--              POLYORB.TASKING.PROFILES.FULL_TASKING.THREADS               --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2002-2006, Free Software Foundation, Inc.          --
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

--  Implementation of Threads under the Full_Tasking profile.

with System.Tasking.Utilities;

with Ada.Real_Time;
with Ada.Unchecked_Deallocation;
with Ada.Unchecked_Conversion;

with PolyORB.Initialization;
with PolyORB.Parameters;
with PolyORB.Utils.Strings;

package body PolyORB.Tasking.Profiles.Full_Tasking.Threads is

   procedure Free is new Ada.Unchecked_Deallocation
     (Full_Tasking_Thread_Type'Class,
      Full_Tasking_Thread_Access);

   procedure Free is new Ada.Unchecked_Deallocation
     (PTT.Runnable_Controller'Class, PTT.Runnable_Controller_Access);

   function A_To_P_Task_Id (ATID : Ada.Task_Identification.Task_Id)
     return PTT.Thread_Id;
   pragma Inline (A_To_P_Task_Id);
   --  Convert Ada Task_Id to PolyORB Task_Id.

   --  Task types.

   task type Generic_Task (P : System.Priority; S : Natural) is
      --  All purpose generic task that executes a 'Runnable'

      pragma Priority (P);
      pragma Storage_Size (S);

      entry Initialize (T : PTT.Thread_Access);
      --  Initialize the task.

      entry Start
        (Run : PTT.Runnable_Access;
         C   : PTT.Runnable_Controller_Access);
      --  Start the task.

   end Generic_Task;

   type Generic_Task_Access is access Generic_Task;

   type Simple_Runnable is new PTT.Runnable with record
      Main_Subprogram : PTT.Parameterless_Procedure;
   end record;
   --  Simplified runnable for parameter less procedure.

   procedure Run (SR : access Simple_Runnable);

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

   procedure Run (SR : access Simple_Runnable)
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

   function Run_In_Task
     (TF               : access Full_Tasking_Thread_Factory_Type;
      Name             : String := "";
      Default_Priority : System.Any_Priority := System.Default_Priority;
      Storage_Size     : Natural := 0;
      R                : PTT.Runnable_Access;
      C                : PTT.Runnable_Controller_Access)
     return PTT.Thread_Access
   is
      pragma Warnings (Off);
      pragma Unreferenced (TF);
      pragma Warnings (On);

      T : constant Full_Tasking_Thread_Access
        := new Full_Tasking_Thread_Type;
      GT : Generic_Task_Access;

   begin
      T.Priority := System.Priority
        (PolyORB.Parameters.Get_Conf
           ("tasking", "polyorb.tasking.threads." & Name & ".priority",
            Default_Priority));

      if Storage_Size = 0 then
         T.Stack_Size := PolyORB.Parameters.Get_Conf
           ("tasking",
            "storage_size",
            PTT.Default_Storage_Size);
      else
         T.Stack_Size := Storage_Size;
      end if;

      GT := new Generic_Task (T.Priority, T.Stack_Size);
      GT.Initialize (PTT.Thread_Access (T));
      GT.Start (R, C);
      return PTT.Thread_Access (T);
   end Run_In_Task;

   function Run_In_Task
     (TF               : access Full_Tasking_Thread_Factory_Type;
      Name             : String := "";
      Default_Priority : System.Any_Priority := System.Default_Priority;
      Storage_Size     : Natural := 0;
      P                : PTT.Parameterless_Procedure)
     return PTT.Thread_Access
   is
      R : constant PTT.Runnable_Access := new Simple_Runnable;

   begin
      Simple_Runnable (R.all).Main_Subprogram := P;

      return Run_In_Task
        (TF,
         Name,
         Default_Priority,
         Storage_Size,
         R,
         new PTT.Runnable_Controller);
   end Run_In_Task;

   ------------------
   -- Generic_Task --
   ------------------

   task body Generic_Task is
      The_Thread     : Full_Tasking_Thread_Access;
      The_Runnable   : PTT.Runnable_Access;
      The_Controller : PTT.Runnable_Controller_Access;
   begin
      accept Initialize (T : PTT.Thread_Access) do
         The_Thread := Full_Tasking_Thread_Access (T);
         The_Thread.Id := A_To_P_Task_Id
           (Ada.Task_Identification.Current_Task);
         --  XXX Maybe The_Thread.Id could be suppressed altogether!!
      end Initialize;

      accept Start
        (Run : PTT.Runnable_Access;
         C : PTT.Runnable_Controller_Access)
      do
         The_Runnable := Run;
         The_Controller := C;
      end Start;

      PTT.Run (The_Runnable);
      PTT.Free_Runnable (The_Controller.all, The_Runnable);
      Free (The_Controller);
      Free (The_Thread);
   end Generic_Task;

   ---------------------------
   -- Get_Current_Thread_Id --
   ---------------------------

   function Get_Current_Thread_Id
     (TF : access Full_Tasking_Thread_Factory_Type)
     return PTT.Thread_Id
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

   function Get_Thread_Id
     (T : access Full_Tasking_Thread_Type)
     return PTT.Thread_Id is
   begin
      return T.Id;
   end Get_Thread_Id;

   -----------
   -- Image --
   -----------

   function Thread_Id_Image
     (TF  : access Full_Tasking_Thread_Factory_Type;
      TID : PTT.Thread_Id) return String
   is
      pragma Warnings (Off);
      pragma Unreferenced (TF);
      pragma Warnings (On);
   begin
      return Ada.Task_Identification.Image (P_To_A_Task_Id (TID));
   end Thread_Id_Image;

   ------------------
   -- Set_Priority --
   ------------------

   procedure Set_Priority
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

   function Get_Priority
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

   procedure Relative_Delay
     (TF : access Full_Tasking_Thread_Factory_Type; D : Duration)
   is
      pragma Unreferenced (TF);
   begin
      delay D;
   end Relative_Delay;

   -----------------
   -- Awake_Count --
   -----------------

   function Awake_Count (TF : access Full_Tasking_Thread_Factory_Type)
     return Natural
   is
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

   function Independent_Count (TF : access Full_Tasking_Thread_Factory_Type)
     return Natural
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
      Time_0 : constant Time := Time_Of (0, Time_Span_Zero);
   begin
      PTT.Node_Boot_Time := To_Duration (Clock - Time_0);
      PTT.Register_Thread_Factory (PTT.Thread_Factory_Access
                                   (The_Thread_Factory));
      The_Thread_Factory.Environment_Task := System.Tasking.Self;
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
