------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--              POLYORB.TASKING.PROFILES.FULL_TASKING.THREADS               --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--         Copyright (C) 2002-2010, Free Software Foundation, Inc.          --
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

--  This package provide an implementation for base types for tasking
--  using full Ada tasking, yet it does not allow for dynamic priority
--  modification of a running thread. For all comments, see
--  PolyORB.Tasking.Threads.

with Ada.Task_Identification;

with PolyORB.Tasking.Threads;

with System;
pragma Warnings (Off);
--  System.Tasking is an internal GNAT unit
with System.Tasking;
pragma Warnings (On);

package PolyORB.Tasking.Profiles.Full_Tasking.Threads is

   package PTT renames PolyORB.Tasking.Threads;

   ------------------------------
   -- Full_Tasking_Thread_Type --
   ------------------------------

   --  Type is declared in package body

   ---------------------------------
   -- Full_Tasking_Thread_Factory --
   ---------------------------------

   type Full_Tasking_Thread_Factory_Type is
     new PTT.Thread_Factory_Type with private;

   type Full_Tasking_Thread_Factory_Access
     is access all Full_Tasking_Thread_Factory_Type'Class;

   The_Thread_Factory : constant Full_Tasking_Thread_Factory_Access;

   function Run_In_Task
     (TF               : access Full_Tasking_Thread_Factory_Type;
      Name             : String := "";
      Default_Priority : System.Any_Priority := System.Default_Priority;
      Storage_Size     : Natural := 0;
      R                : PTT.Runnable_Access) return PTT.Thread_Access;

   function Run_In_Task
     (TF               : access Full_Tasking_Thread_Factory_Type;
      Name             : String := "";
      Default_Priority : System.Any_Priority := System.Default_Priority;
      Storage_Size     : Natural := 0;
      P                : PTT.Parameterless_Procedure) return PTT.Thread_Access;

   function Get_Current_Thread_Id
     (TF : access Full_Tasking_Thread_Factory_Type)
     return PTT.Thread_Id;

   function P_To_A_Task_Id (TID : PTT.Thread_Id)
     return Ada.Task_Identification.Task_Id;
   pragma Inline (P_To_A_Task_Id);
   --  Convert PolyORB Task_Id to Ada Task_Id.

   function Thread_Id_Image
     (TF : access Full_Tasking_Thread_Factory_Type;
      TID : PTT.Thread_Id)
     return String;

   procedure Set_Priority
     (TF : access Full_Tasking_Thread_Factory_Type;
      T  :        PTT.Thread_Id;
      P  :        System.Any_Priority);

   function Get_Priority
     (TF : access Full_Tasking_Thread_Factory_Type;
      T  :        PTT.Thread_Id)
     return System.Any_Priority;

   procedure Relative_Delay
     (TF : access Full_Tasking_Thread_Factory_Type; D : Duration);

   function Awake_Count (TF : access Full_Tasking_Thread_Factory_Type)
     return Natural;

   function Independent_Count (TF : access Full_Tasking_Thread_Factory_Type)
     return Natural;

private

   type Full_Tasking_Thread_Factory_Type is new PTT.Thread_Factory_Type
   with record
      Environment_Task : System.Tasking.Task_Id;
      --  The environment task
   end record;

   The_Thread_Factory : constant Full_Tasking_Thread_Factory_Access :=
                          new Full_Tasking_Thread_Factory_Type;

   type Set_Priority_Hook is access procedure
     (TF : access Full_Tasking_Thread_Factory_Type;
      T  : PTT.Thread_Id;
      P  : System.Any_Priority);

   Set_Priority_P : Set_Priority_Hook;

   type Get_Priority_Hook is access function
     (TF : access Full_Tasking_Thread_Factory_Type;
      T  : PTT.Thread_Id) return System.Any_Priority;

   Get_Priority_P : Get_Priority_Hook;

end PolyORB.Tasking.Profiles.Full_Tasking.Threads;
