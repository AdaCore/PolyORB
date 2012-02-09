------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--               POLYORB.TASKING.PROFILES.NO_TASKING.THREADS                --
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

pragma Ada_2005;

--  Implementation of Threads under the No_Tasking profile

with Ada.Calendar;
with Ada.Unchecked_Conversion;

with PolyORB.Initialization;
with PolyORB.Utils.Strings;

package body PolyORB.Tasking.Profiles.No_Tasking.Threads is

   --  For the no-tasking profile, there is only one valid Thread_Id,
   --  Main_Thread_Id, which is arbitrarily defined but must be different from
   --  Null_Thread_Id.

   function To_Thread_Id is
     new Ada.Unchecked_Conversion (System.Address, PTT.Thread_Id);
   Main_Thread_Id : constant PTT.Thread_Id :=
                      To_Thread_Id (The_Thread_Factory'Address);

   ---------------------------
   -- Get_Current_Thread_Id --
   ---------------------------

   overriding function Get_Current_Thread_Id
     (TF : access No_Tasking_Thread_Factory_Type)
     return PTT.Thread_Id
   is
      pragma Warnings (Off);
      pragma Unreferenced (TF);
      pragma Warnings (On);
   begin
      return Main_Thread_Id;
   end Get_Current_Thread_Id;

   -------------------
   -- Get_Thread_Id --
   -------------------

   overriding function Get_Thread_Id
     (T : access No_Tasking_Thread_Type)
     return PTT.Thread_Id
   is
      pragma Warnings (Off);
      pragma Unreferenced (T);
      pragma Warnings (On);
   begin
      return Main_Thread_Id;
   end Get_Thread_Id;

   ---------------------
   -- Thread_Id_Image --
   ---------------------

   overriding function Thread_Id_Image
     (TF  : access No_Tasking_Thread_Factory_Type;
      TID : PTT.Thread_Id)
      return String
   is
      use PTT;

      pragma Warnings (Off);
      pragma Unreferenced (TF);
      pragma Warnings (On);
   begin
      if TID = Null_Thread_Id then
         return "<null thread id>";
      else
         return "main_task";
      end if;
   end Thread_Id_Image;

   -----------------
   -- Run_In_Task --
   -----------------

   overriding function Run_In_Task
     (TF               : access No_Tasking_Thread_Factory_Type;
      Name             : String;
      Default_Priority : System.Any_Priority := System.Default_Priority;
      Storage_Size     : Natural := 0;
      R                : PTT.Runnable_Access)
     return PTT.Thread_Access
   is
      pragma Warnings (Off);
      pragma Unreferenced (TF);
      pragma Unreferenced (Name);
      pragma Unreferenced (Default_Priority);
      pragma Unreferenced (Storage_Size);
      pragma Unreferenced (R);
      pragma Warnings (On);
   begin
      raise Tasking_Error;
      return null;
   end Run_In_Task;

   overriding function Run_In_Task
     (TF               : access No_Tasking_Thread_Factory_Type;
      Name             : String;
      Default_Priority : System.Any_Priority := System.Default_Priority;
      Storage_Size     : Natural := 0;
      P                : PTT.Parameterless_Procedure) return PTT.Thread_Access
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

   overriding procedure Set_Priority
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

   overriding function Get_Priority
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

   overriding procedure Relative_Delay
     (TF : access No_Tasking_Thread_Factory_Type; D : Duration)
   is
      pragma Unreferenced (TF);
   begin
      delay D;
   end Relative_Delay;

   -----------------
   -- Awake_Count --
   -----------------

   overriding function Awake_Count (TF : access No_Tasking_Thread_Factory_Type)
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

   overriding function Independent_Count
     (TF : access No_Tasking_Thread_Factory_Type)
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
