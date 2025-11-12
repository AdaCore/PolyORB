------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--              POLYORB.TASKING.PROFILES.FULL_TASKING.MUTEXES               --
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

--  Implementation of POSIX-like mutexes with full Ada tasking.
--  This variant uses GNAT-specific library facilities.

--  WAG:601
--  pragma Warnings (Off) with pattern not supported in that compiler version
--  so use plain pragma Warnings (Off/On) instead.
--  pragma Warnings (Off, "* is an internal GNAT unit");
--  pragma Warnings (Off, "use of this unit is non-portable*");

pragma Warnings (Off);
--  Depends on System.Task_Primitives.Operations, an internal GNAT unit
with System.Task_Primitives.Operations;
pragma Warnings (On);

with System;

with PolyORB.Utils.Unchecked_Deallocation;

with PolyORB.Initialization;
with PolyORB.Log;
with PolyORB.Utils.Strings;

package body PolyORB.Tasking.Profiles.Full_Tasking.Mutexes is

   use System.Task_Primitives.Operations;
   use PolyORB.Log;

   package L is new PolyORB.Log.Facility_Log
     ("polyorb.tasking.profiles.full_tasking.mutexes");
   procedure O (Message : String; Level : Log_Level := Debug)
     renames L.Output;
   function C (Level : Log_Level := Debug) return Boolean
     renames L.Enabled;

   ----------
   -- Free --
   ----------

   procedure Free is new PolyORB.Utils.Unchecked_Deallocation.Free
     (Object => PTM.Mutex_Type'Class,
      Name => PTM.Mutex_Access);

   procedure Free is new PolyORB.Utils.Unchecked_Deallocation.Free
     (Object => Mutex_Lock,
      Name => Mutex_Lock_Access);

   ------------
   -- Create --
   ------------

   overriding function Create
     (MF   : access Full_Tasking_Mutex_Factory_Type;
      Name : String := "")
      return PTM.Mutex_Access
   is
      pragma Warnings (Off);
      pragma Unreferenced (MF);

      pragma Unreferenced (Name);
      --  XXX The use of Name is not yet implemented

      pragma Warnings (On);

      M : constant Full_Tasking_Mutex_Access := new Full_Tasking_Mutex_Type;
   begin
      pragma Debug (C, O ("Create Mutex"));
      M.The_Lock := new Mutex_Lock;
      Initialize_Lock (Prio => System.Any_Priority'Last, L => M.The_Lock);
      return PTM.Mutex_Access (M);
   end Create;

   -------------
   -- Destroy --
   -------------

   overriding procedure Destroy
     (MF : access Full_Tasking_Mutex_Factory_Type;
      M  : in out PTM.Mutex_Access)
   is
      pragma Warnings (Off);
      pragma Unreferenced (MF);
      pragma Warnings (On);

   begin
      pragma Debug (C, O ("Destroy mutex"));
      Finalize_Lock (Full_Tasking_Mutex_Access (M).The_Lock);
      Free (Full_Tasking_Mutex_Access (M).The_Lock);
      Free (M);
   end Destroy;

   -----------
   -- Enter --
   -----------

   overriding procedure Enter (M : access Full_Tasking_Mutex_Type) is
      Ceiling_Violation : Boolean;
   begin
      pragma Debug (C, O ("Enter mutex"));
      Write_Lock (M.The_Lock, Ceiling_Violation);
      if Ceiling_Violation then
         raise Program_Error;
      end if;
   end Enter;

   -----------
   -- Leave --
   -----------

   overriding procedure Leave (M : access Full_Tasking_Mutex_Type) is
   begin
      pragma Debug (C, O ("Leave mutex"));
      Unlock (M.The_Lock);
   end Leave;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize;

   procedure Initialize is
   begin
      pragma Debug (C, O ("Initialize package Profiles.Full_Tasking.Mutexes"));
      PTM.Register_Mutex_Factory (PTM.Mutex_Factory_Access
                                    (The_Mutex_Factory));
   end Initialize;

   use PolyORB.Initialization;
   use PolyORB.Initialization.String_Lists;
   use PolyORB.Utils.Strings;

begin
   Register_Module
     (Module_Info'
      (Name      => +"tasking.profiles.full_tasking.mutexes",
       Conflicts => Empty,
       Depends   => Empty,
       Provides  => +"tasking.mutexes",
       Implicit  => False,
       Init      => Initialize'Access,
       Shutdown  => null));
end PolyORB.Tasking.Profiles.Full_Tasking.Mutexes;
