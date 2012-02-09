------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--         POLYORB.TASKING.PROFILES.RAVENSCAR.THREADS.ANNOTATIONS           --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2004-2012, Free Software Foundation, Inc.          --
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

with PolyORB.Utils.Strings;

with PolyORB.Utils.HFunctions.Hyper;
with PolyORB.Utils.HTables.Perfect;
with PolyORB.Tasking.Mutexes;
with PolyORB.Tasking.Threads;

package body PolyORB.Tasking.Profiles.Ravenscar.Threads.Annotations is

   use PolyORB.Annotations;
   use PolyORB.Tasking.Mutexes;

   Current_TAF : Ravenscar_TAF_Access;

   package HTable is
      new PolyORB.Utils.HTables.Perfect
     (Notepad_Access,
      PolyORB.Utils.HFunctions.Hyper.Hash_Hyper_Parameters,
      PolyORB.Utils.HFunctions.Hyper.Default_Hash_Parameters,
      PolyORB.Utils.HFunctions.Hyper.Hash,
      PolyORB.Utils.HFunctions.Hyper.Next_Hash_Parameters);
   use HTable;

   Map : Table_Instance;
   Mutex : Mutex_Access;
   --  Implementation Note: in this implementation, we rely on the
   --  fact that we use the Ravenscar profile: there is no task
   --  finalization. Thus, we do not have to care about the
   --  deallocation of the elements stored in Map.

   --------------------------------
   -- Get_Current_Thread_Notepad --
   --------------------------------

   overriding function Get_Current_Thread_Notepad
     (TAF : access Ravenscar_TAF)
     return PolyORB.Annotations.Notepad_Access
   is
      pragma Unreferenced (TAF);

      Task_Key : constant String := Image (Current_Task);
      Note     : Notepad_Access;
   begin
      Enter (Mutex);
      Note := Lookup (Map, Task_Key, null);

      if Note = null then
         Note := new Notepad;
         Insert (Map, Task_Key, Note);
      end if;

      Leave (Mutex);

      return Note;
   end Get_Current_Thread_Notepad;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is
   begin
      Current_TAF := new Ravenscar_TAF;
      Initialize (Map);
      Create (Mutex);

      PolyORB.Tasking.Threads.Annotations.Register
        (PolyORB.Tasking.Threads.Annotations.TAF_Access (Current_TAF));
   end Initialize;

   use PolyORB.Initialization;
   use PolyORB.Initialization.String_Lists;
   use PolyORB.Utils.Strings;

begin
   Register_Module
     (Module_Info'
      (Name      => +"tasking.profiles.ravenscar.annotations",
       Conflicts => Empty,
       Depends   => +"tasking.mutexes",
       Provides  => +"tasking.annotations",
       Implicit  => False,
       Init      => Initializer,
       Shutdown  => null));
end PolyORB.Tasking.Profiles.Ravenscar.Threads.Annotations;
