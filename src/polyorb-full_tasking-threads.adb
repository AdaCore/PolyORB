------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--         P O L Y O R B . F U L L . T A S K I N G . T H R E A D S          --
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

--  $Id$

with System;
with Ada.Task_Identification;
with Unchecked_Deallocation;
with PolyORB.Configuration;

with PolyORB.Initialization;
with PolyORB.Utils.Strings;

package body PolyORB.Full_Tasking.Threads is

   procedure Initialize;

   procedure Free is new Unchecked_Deallocation
     (Full_Tasking_Thread_Type'Class,
      Full_Tasking_Thread_Access);

   procedure Free is new Unchecked_Deallocation
     (Full_Tasking_Thread_Id'Class,
      Full_Tasking_Thread_Id_Access);

   procedure Free is new Unchecked_Deallocation
     (PTT.Runnable'Class, PTT.Runnable_Access);

   task type Generic_Task (P : System.Priority) is
      --  type of the tasks created by this package.

      pragma Priority (P);

      entry Initialize (T : PTT.Thread_Access);
      --  Give the task its parameters.

      entry Start (Run : PTT.Runnable_Access);
      --  Start the task.

      pragma Storage_Size (131072);
   end Generic_Task;

   type Generic_Task_Access is access Generic_Task;

   ---------
   -- "=" --
   ---------

   function "="
     (T1 : Full_Tasking_Thread_Id;
      T2 : Full_Tasking_Thread_Id)
     return Boolean is
      use Ada.Task_Identification;
   begin
      return T1.Tid = T2.Tid;
   end "=";

   --------------------
   -- Copy_Thread_Id --
   --------------------

   procedure Copy_Thread_Id
     (TF     : access Full_Tasking_Thread_Factory_Type;
      Source : PTT.Thread_Id'Class;
      Target : PTT.Thread_Id_Access) is
      pragma Warnings (Off);
      pragma Unreferenced (TF);
      pragma Warnings (On);
      Result  : Full_Tasking_Thread_Id_Access
        := Full_Tasking_Thread_Id_Access (Target);
      Content : Full_Tasking_Thread_Id
        := Full_Tasking_Thread_Id (Source);
   begin
      Result.Tid := Content.Tid;
   end Copy_Thread_Id;

   -------------------
   -- Create_Thread --
   -------------------

   function Create_Thread
     (TF               : access Full_Tasking_Thread_Factory_Type;
      Name             : String := "";
      Default_Priority : System.Any_Priority := System.Default_Priority;
      R                : access PTT.Runnable'Class)
     return PTT.Thread_Access is
      pragma Warnings (Off);
      pragma Unreferenced (TF);
      pragma Warnings (On);
      T : Full_Tasking_Thread_Access
        := new Full_Tasking_Thread_Type;
   begin
      T.Run := new PTT.Runnable'Class'(R.all);
      T.Priority := System.Priority
        (PolyORB.Configuration.Get_Conf
           ("tasking", "polyorb.tasking.threads." & Name & ".priority",
            Default_Priority));
      return PTT.Thread_Access (T);
   end Create_Thread;

   -----------------
   -- Create_Task --
   -----------------

   procedure Create_Task
     (TF : in out  Full_Tasking_Thread_Factory_Type;
      T  : access PTT.Thread_Type'Class) is
      GT : Generic_Task_Access;
      pragma Warnings (Off);
      pragma Unreferenced (TF);
      pragma Warnings (On);
   begin
      GT := new Generic_Task (System.Default_Priority);
      GT.Initialize (PTT.Thread_Access (T));
      GT.Start (Full_Tasking_Thread_Access (T).Run);
   end Create_Task;

   ------------------
   -- Generic_Task --
   ------------------

   task body Generic_Task is
      The_Thread   : Full_Tasking_Thread_Access;
   begin
      accept Initialize (T : PTT.Thread_Access) do
         The_Thread := Full_Tasking_Thread_Access (T);
         The_Thread.Id
           := new Full_Tasking_Thread_Id'
           (Tid => Ada.Task_Identification.Current_Task);
      end Initialize;
      accept Start (Run : PTT.Runnable_Access) do
         The_Thread.Run := Run;
      end Start;
      PTT.Run (The_Thread.Run);
      Free (Full_Tasking_Thread_Id_Access (The_Thread.Id));
      Free (The_Thread.Run);
      Free (The_Thread);
   end Generic_Task;

   ---------------------------
   -- Get_Current_Thread_Id --
   ---------------------------

   function Get_Current_Thread_Id
     (TF : access Full_Tasking_Thread_Factory_Type)
     return PTT.Thread_Id'Class is
      pragma Warnings (Off);
      pragma Unreferenced (TF);
      pragma Warnings (On);
      T : Full_Tasking_Thread_Id;
   begin
      T.Tid := Ada.Task_Identification.Current_Task;
      return PTT.Thread_Id'Class (T);
   end Get_Current_Thread_Id;

   -------------------
   -- Get_Thread_Id --
   -------------------

   function Get_Thread_Id
     (T : access Full_Tasking_Thread_Type)
     return PTT.Thread_Id_Access is
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

   function Image (T : Full_Tasking_Thread_Id) return String is
      use Ada.Task_Identification;
   begin
      return Image (T.Tid);
   end Image;

   ---------
   -- Run --
   ---------

   procedure Run (T : access Full_Tasking_Thread_Type) is
   begin
      PTT.Run (T.Run);
   end Run;

   use PolyORB.Initialization;
   use PolyORB.Initialization.String_Lists;
   use PolyORB.Utils.Strings;


begin
   Register_Module
     (Module_Info'
      (Name => +"full_tasking-threads",
       Conflicts => Empty,
       Depends => Empty,
       Provides => +"tasking-threads",
       Init => Initialize'Access));
end PolyORB.Full_Tasking.Threads;
