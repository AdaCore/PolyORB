------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--              POLYORB.TASKING.PROFILES.FULL_TASKING.MUTEXES               --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2002-2008, Free Software Foundation, Inc.          --
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

with Ada.Unchecked_Deallocation;

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

   procedure Free is new Ada.Unchecked_Deallocation
     (PTM.Mutex_Type'Class, PTM.Mutex_Access);

   procedure Free is new Ada.Unchecked_Deallocation
     (Mutex_Lock, Mutex_Lock_Access);

   ------------
   -- Create --
   ------------

   function Create
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

   procedure Destroy
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

   procedure Enter (M : access Full_Tasking_Mutex_Type) is
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

   procedure Leave (M : access Full_Tasking_Mutex_Type) is
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
