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

--  Implementation of Threads under the No_Tasking profile.


with PolyORB.Initialization;
with PolyORB.Utils.Strings;

package body PolyORB.No_Tasking_Profile.Threads is

   procedure Initialize;

   ---------
   -- "=" --
   ---------

   function "="
     (T1 : No_Tasking_Thread_Id;
      T2 : No_Tasking_Thread_Id)
     return Boolean is
      pragma Warnings (Off);
      pragma Unreferenced (T1);
      pragma Unreferenced (T2);
      pragma Warnings (On);
   begin
      return True;
   end "=";

   --------------------
   -- Copy_Thread_Id --
   --------------------

   procedure Copy_Thread_Id
     (TF     : access No_Tasking_Thread_Factory_Type;
      Source : PTT.Thread_Id'Class;
      Target : PTT.Thread_Id_Access) is
      pragma Warnings (Off);
      pragma Unreferenced (TF);
      pragma Unreferenced (Source);
      pragma Unreferenced (Target);
      pragma Warnings (On);
   begin
      null;
   end Copy_Thread_Id;

   -------------------
   -- Create_Thread --
   -------------------

   function Create_Thread
     (TF               : access No_Tasking_Thread_Factory_Type;
      Name             : String := "";
      Default_Priority : System.Any_Priority := System.Default_Priority;
      R                : access PTT.Runnable'Class)
     return PTT.Thread_Access is
      pragma Warnings (Off);
      pragma Unreferenced (TF);
      pragma Unreferenced (Name);
      pragma Unreferenced (Default_Priority);
      pragma Unreferenced (R);
      pragma Warnings (On);
   begin
      return null;
   end Create_Thread;

   -----------------
   -- Create_Task --
   -----------------

   procedure Create_Task
     (TF : in out  No_Tasking_Thread_Factory_Type;
      T  : access PTT.Thread_Type'Class) is
      pragma Warnings (Off);
      pragma Unreferenced (TF);
      pragma Unreferenced (T);
      pragma Warnings (On);
   begin
      raise Not_Implemented;
   end Create_Task;

   ---------------------------
   -- Get_Current_Thread_Id --
   ---------------------------

   function Get_Current_Thread_Id
     (TF : access No_Tasking_Thread_Factory_Type)
     return PTT.Thread_Id'Class is
      pragma Warnings (Off);
      pragma Unreferenced (TF);
      pragma Warnings (On);
      T : No_Tasking_Thread_Id;
   begin
      return T;
   end Get_Current_Thread_Id;

   -------------------
   -- Get_Thread_Id --
   -------------------

   function Get_Thread_Id
     (T : access No_Tasking_Thread_Type)
     return PTT.Thread_Id_Access is
      pragma Warnings (Off);
      pragma Unreferenced (T);
      pragma Warnings (On);
   begin
      return new No_Tasking_Thread_Id;
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

   function Image (T : No_Tasking_Thread_Id) return String is
      pragma Warnings (Off);
      pragma Unreferenced (T);
      pragma Warnings (On);
   begin
      return "task_id";
   end Image;

   ---------
   -- Run --
   ---------

   procedure Run (T : access No_Tasking_Thread_Type) is
      pragma Warnings (Off);
      pragma Unreferenced (T);
      pragma Warnings (On);
   begin
      raise Not_Implemented;
   end Run;

   use PolyORB.Initialization;
   use PolyORB.Initialization.String_Lists;
   use PolyORB.Utils.Strings;

begin
   Register_Module
     (Module_Info'
      (Name => +"no_tasking_profile-threads",
       Conflicts => Empty,
       Depends => Empty,
       Provides => +"tasking-threads",
       Init => Initialize'Access));
end PolyORB.No_Tasking_Profile.Threads;
