------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--               POLYORB.TASKING.PROFILES.NO_TASKING.THREADS                --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2002-2007, Free Software Foundation, Inc.          --
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

--  Implementation of Threads under the No_Tasking profile

with PolyORB.Initialization;
with PolyORB.Utils.Strings;

with Ada.Calendar;

package body PolyORB.Tasking.Profiles.No_Tasking.Threads is

   ---------------------------
   -- Get_Current_Thread_Id --
   ---------------------------

   function Get_Current_Thread_Id
     (TF : access No_Tasking_Thread_Factory_Type)
     return PTT.Thread_Id
   is
      pragma Warnings (Off);
      pragma Unreferenced (TF);
      pragma Warnings (On);
   begin
      return PTT.Null_Thread_Id;
   end Get_Current_Thread_Id;

   -------------------
   -- Get_Thread_Id --
   -------------------

   function Get_Thread_Id
     (T : access No_Tasking_Thread_Type)
     return PTT.Thread_Id
   is
      pragma Warnings (Off);
      pragma Unreferenced (T);
      pragma Warnings (On);
   begin
      return PTT.Null_Thread_Id;
   end Get_Thread_Id;

   ---------------------
   -- Thread_Id_Image --
   ---------------------

   function Thread_Id_Image
     (TF  : access No_Tasking_Thread_Factory_Type;
      TID : PTT.Thread_Id)
      return String
   is
      pragma Warnings (Off);
      pragma Unreferenced (TF, TID);
      pragma Warnings (On);
   begin
      return "main_task";
   end Thread_Id_Image;

   -----------------
   -- Run_In_Task --
   -----------------

   function Run_In_Task
     (TF               : access No_Tasking_Thread_Factory_Type;
      Name             : String := "";
      Default_Priority : System.Any_Priority := System.Default_Priority;
      Storage_Size     : Natural := 0;
      R                : PTT.Runnable_Access;
      C                : PTT.Runnable_Controller_Access)
     return PTT.Thread_Access
   is
      pragma Warnings (Off);
      pragma Unreferenced (TF);
      pragma Unreferenced (Name);
      pragma Unreferenced (Default_Priority);
      pragma Unreferenced (Storage_Size);
      pragma Unreferenced (R);
      pragma Unreferenced (C);
      pragma Warnings (On);
   begin
      raise Tasking_Error;
      return null;
   end Run_In_Task;

   function Run_In_Task
     (TF               : access No_Tasking_Thread_Factory_Type;
      Name             : String := "";
      Default_Priority : System.Any_Priority := System.Default_Priority;
      Storage_Size     : Natural := 0;
      P                : PTT.Parameterless_Procedure)
     return PTT.Thread_Access
   is
      pragma Warnings (Off);
      pragma Unreferenced (TF);
      pragma Unreferenced (Name);
      pragma Unreferenced (Default_Priority);
      pragma Unreferenced (Storage_Size);
      pragma Unreferenced (P);
      pragma Warnings (On);
   begin
      raise Tasking_Error;
      return null;
   end Run_In_Task;

   ------------------
   -- Set_Priority --
   ------------------

   procedure Set_Priority
     (TF : access No_Tasking_Thread_Factory_Type;
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
     (TF : access No_Tasking_Thread_Factory_Type;
      T  :        PTT.Thread_Id)
     return System.Any_Priority
   is
      pragma Warnings (Off);
      pragma Unreferenced (TF);
      pragma Unreferenced (T);
      pragma Warnings (On);
   begin
      raise Tasking_Error;

      return 0;
   end Get_Priority;

   --------------------
   -- Relative_Delay --
   --------------------

   procedure Relative_Delay
     (TF : access No_Tasking_Thread_Factory_Type; D : Duration)
   is
      pragma Unreferenced (TF);
   begin
      delay D;
   end Relative_Delay;

   -----------------
   -- Awake_Count --
   -----------------

   function Awake_Count (TF : access No_Tasking_Thread_Factory_Type)
     return Natural
   is
      pragma Unreferenced (TF);
   begin

      --  With the no tasking profile we can assume that there is always one
      --  awaken task if this function is called.

      return 1;
   end Awake_Count;

   -----------------------
   -- Independent_Count --
   -----------------------

   function Independent_Count (TF : access No_Tasking_Thread_Factory_Type)
     return Natural
   is
      pragma Unreferenced (TF);
   begin
      return 0;
   end Independent_Count;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize;

   procedure Initialize is
      use Ada.Calendar;
      Epoch : constant Time := Time_Of (1970, 1, 1);
   begin
      PTT.Node_Boot_Time := Clock - Epoch;
      PTT.Register_Thread_Factory (PTT.Thread_Factory_Access
                                   (The_Thread_Factory));
   end Initialize;

   use PolyORB.Initialization;
   use PolyORB.Initialization.String_Lists;
   use PolyORB.Utils.Strings;

begin
   Register_Module
     (Module_Info'
      (Name      => +"tasking.profiles.no_tasking.threads",
       Conflicts => Empty,
       Depends   => Empty,
       Provides  => +"tasking.threads",
       Implicit  => False,
       Init      => Initialize'Access,
       Shutdown  => null));
end PolyORB.Tasking.Profiles.No_Tasking.Threads;
