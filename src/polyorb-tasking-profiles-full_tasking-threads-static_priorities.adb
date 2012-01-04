------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--     POLYORB.TASKING.PROFILES.FULL_TASKING.THREADS.STATIC_PRIORITIES      --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2003-2012, Free Software Foundation, Inc.          --
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

with PolyORB.Initialization;

with PolyORB.Utils.Strings;

package body PolyORB.Tasking.Profiles.Full_Tasking.Threads.Static_Priorities
is

   ------------------
   -- Set_Priority --
   ------------------

   procedure Set_Priority
     (TF : access Full_Tasking_Thread_Factory_Type;
      T  :        PTT.Thread_Id;
      P  :        System.Any_Priority)
   is
      pragma Unreferenced (TF);
      pragma Unreferenced (T);
      pragma Unreferenced (P);

   begin
      raise Tasking_Error;
   end Set_Priority;

   ------------------
   -- Get_Priority --
   ------------------

   function Get_Priority
     (TF : access Full_Tasking_Thread_Factory_Type;
      T  :        PTT.Thread_Id)
     return System.Any_Priority
   is
      pragma Unreferenced (TF);
      pragma Unreferenced (T);

   begin
      raise Tasking_Error;

      return 0;
   end Get_Priority;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize;

   procedure Initialize is
   begin
      PolyORB.Tasking.Profiles.Full_Tasking.Threads.Set_Priority_P
        := Set_Priority'Access;

      PolyORB.Tasking.Profiles.Full_Tasking.Threads.Get_Priority_P
        := Get_Priority'Access;
   end Initialize;

   use PolyORB.Initialization;
   use PolyORB.Initialization.String_Lists;
   use PolyORB.Utils.Strings;

begin
   Register_Module
     (Module_Info'
      (Name      => +"tasking.profiles.full_tasking.thread.static_priorities",
       Conflicts => Empty,
       Depends   => Empty,
       Provides  => +"full_tasking.threads.priorities",
       Implicit  => False,
       Init      => Initialize'Access,
       Shutdown  => null));
end PolyORB.Tasking.Profiles.Full_Tasking.Threads.Static_Priorities;
