------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--               POLYORB.TASKING.PROFILES.NO_TASKING.MUTEXES                --
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

--  Implementation of POSIX-like mutexes under the No_Tasking profile.

with PolyORB.Initialization;

with PolyORB.Utils.Strings;

package body PolyORB.Tasking.Profiles.No_Tasking.Mutexes is

   The_Mutex : aliased No_Tasking_Mutex_Type;

   ------------
   -- Create --
   ------------

   overriding function Create
     (MF   : access No_Tasking_Mutex_Factory_Type;
      Name : String := "")
     return PTM.Mutex_Access is
      pragma Warnings (Off);
      pragma Unreferenced (MF);
      pragma Unreferenced (Name);
      pragma Warnings (On);
   begin
      return The_Mutex'Access;
   end Create;

   -------------
   -- Destroy --
   -------------

   overriding procedure Destroy
     (MF : access No_Tasking_Mutex_Factory_Type;
      M  : in out PTM.Mutex_Access) is
      pragma Warnings (Off);
      pragma Unreferenced (MF);
      pragma Warnings (On);
   begin
      M := null;
   end Destroy;

   -----------
   -- Enter --
   -----------

   overriding procedure Enter (M : access No_Tasking_Mutex_Type)
   is
      pragma Warnings (Off);
      pragma Unreferenced (M);
      pragma Warnings (On);
   begin
      null;
   end Enter;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is
   begin
      PTM.Register_Mutex_Factory (PTM.Mutex_Factory_Access
                                    (The_Mutex_Factory));
   end Initialize;

   -----------
   -- Leave --
   -----------

   overriding procedure Leave (M : access No_Tasking_Mutex_Type)
   is
      pragma Warnings (Off);
      pragma Unreferenced (M);
      pragma Warnings (On);
   begin
      null;
   end Leave;

   use PolyORB.Initialization;
   use PolyORB.Initialization.String_Lists;
   use PolyORB.Utils.Strings;

begin
   Register_Module
     (Module_Info'
      (Name      => +"tasking.profiles.no_tasking.mutexes",
       Conflicts => Empty,
       Depends   => Empty,
       Provides  => +"tasking.mutexes",
       Implicit  => False,
       Init      => Initialize'Access,
       Shutdown  => null));
end PolyORB.Tasking.Profiles.No_Tasking.Mutexes;
