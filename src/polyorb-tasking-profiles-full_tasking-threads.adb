------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--             P O L Y O R B . T A S K I N G . P R O F I L E S              --
--                . F U L L _ T A S K I N G . T H R E A D S                 --
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

--  Implementation of Threads under the Full_Tasking profile.

with System;
with System.Tasking;

with Ada.Task_Identification;
with Ada.Dynamic_Priorities;
with Ada.Unchecked_Deallocation;
with Ada.Unchecked_Conversion;

with PolyORB.Configuration;
with PolyORB.Initialization;
with PolyORB.Utils.Strings;

package body PolyORB.Tasking.Profiles.Full_Tasking.Threads is

   procedure Initialize;

   procedure Free is new Ada.Unchecked_Deallocation
     (Full_Tasking_Thread_Type'Class,
      Full_Tasking_Thread_Access);

   procedure Free is new Ada.Unchecked_Deallocation
     (PTT.Runnable_Controller'Class, PTT.Runnable_Controller_Access);

   function P_To_A_Task_Id (TID : PTT.Thread_Id)
     return Ada.Task_Identification.Task_Id;
   pragma Inline (P_To_A_Task_Id);
   --  Convert PolyORB Task_Id to Ada Task_Id.

   function A_To_P_Task_Id (ATID : Ada.Task_Identification.Task_Id)
     return PTT.Thread_Id;
   pragma Inline (A_To_P_Task_Id);
   --  Convert Ada Task_Id to PolyORB Task_Id.

   function P_To_A_Task_Id (TID : PTT.Thread_Id)
     return Ada.Task_Identification.Task_Id
   is
      function STID_To_ATID is new Ada.Unchecked_Conversion
        (System.Tasking.Task_Id, Ada.Task_Identification.Task_Id);
   begin
      return STID_To_ATID (System.Tasking.To_Task_Id (PTT.To_Address (TID)));
   end P_To_A_Task_Id;

   function A_To_P_Task_Id (ATID : Ada.Task_Identification.Task_Id)
     return PTT.Thread_Id
   is
      function ATID_To_STID is new Ada.Unchecked_Conversion
        (Ada.Task_Identification.Task_Id, System.Tasking.Task_Id);
   begin
      return PTT.To_Thread_Id (System.Tasking.To_Address (ATID_To_STID (ATID)));
   end A_To_P_Task_Id;

   task type Generic_Task_With_Runnable (P : System.Priority) is
      --  type of the tasks created by this package;
      --  this one use a Runnable for its code.

      pragma Priority (P);

      entry Initialize (T : PTT.Thread_Access);
      --  Give the task its parameters.

      entry Start
        (Run : PTT.Runnable_Access;
         C   : PTT.Runnable_Controller_Access);
      --  Start the task.

      pragma Storage_Size (131072);
   end Generic_Task_With_Runnable;

   type Generic_Task_With_Runnable_Access is access Generic_Task_With_Runnable;

   procedure Run (SR : access Simple_Runnable) is
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
      R                : PTT.Runnable_Access;
      C                : PTT.Runnable_Controller_Access)
     return PTT.Thread_Access is
      pragma Warnings (Off);
      pragma Unreferenced (TF);
      pragma Warnings (On);
      T            : Full_Tasking_Thread_Access
        := new Full_Tasking_Thread_Type;
      GT           : Generic_Task_With_Runnable_Access;
   begin
      T.Priority := System.Priority
        (PolyORB.Configuration.Get_Conf
           ("tasking", "polyorb.tasking.threads." & Name & ".priority",
            Default_Priority));
      GT := new Generic_Task_With_Runnable (T.Priority);
      GT.Initialize (PTT.Thread_Access (T));
      GT.Start (R, C);
      return PTT.Thread_Access (T);
   end Run_In_Task;

   function Run_In_Task
     (TF               : access Full_Tasking_Thread_Factory_Type;
      Name             : String := "";
      Default_Priority : System.Any_Priority := System.Default_Priority;
      P                : PTT.Parameterless_Procedure)
     return PTT.Thread_Access
   is
      pragma Warnings (Off);
      pragma Unreferenced (TF);
      pragma Warnings (On);
      R : constant PTT.Runnable_Access := new Simple_Runnable;
   begin
      Simple_Runnable (R.all).Main_Subprogram := P;
      return Run_In_Task
        (TF, Name, Default_Priority, R, new PTT.Runnable_Controller);
   end Run_In_Task;

   --------------------------------
   -- Generic_Task_With_Runnable --
   --------------------------------

   task body Generic_Task_With_Runnable is
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
   end Generic_Task_With_Runnable;

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

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is
   begin
      PTT.Register_Thread_Factory (PTT.Thread_Factory_Access
                                   (The_Thread_Factory));
   end Initialize;

   -----------
   -- Image --
   -----------

   function Thread_Id_Image
     (TF  : access Full_Tasking_Thread_Factory_Type;
      TID : PTT.Thread_Id) return String is
   begin
      return Ada.Task_Identification.Image (P_To_A_Task_Id (TID));
   end Thread_Id_Image;

   ------------------
   -- Set_Priority --
   ------------------

   procedure Set_Priority
     (TF : access Full_Tasking_Thread_Factory_Type;
      T  : PTT.Thread_Id;
      P  : System.Any_Priority)
   is
      pragma Warnings (Off);
      pragma Unreferenced (TF);
      pragma Warnings (On);

      function To_Ada_Task_Id is new Ada.Unchecked_Conversion
        (System.Tasking.Task_Id, Ada.Task_Identification.Task_Id);
   begin
      Ada.Dynamic_Priorities.Set_Priority (P, P_To_A_Task_Id (T));
   end Set_Priority;

   use PolyORB.Initialization;
   use PolyORB.Initialization.String_Lists;
   use PolyORB.Utils.Strings;

begin
   Register_Module
     (Module_Info'
      (Name => +"tasking.profiles.full_tasking.threads",
       Conflicts => Empty,
       Depends => Empty,
       Provides => +"tasking.threads",
       Init => Initialize'Access));
end PolyORB.Tasking.Profiles.Full_Tasking.Threads;
