------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--         POLYORB.TASKING.PROFILES.NO_TASKING.CONDITION_VARIABLES          --
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

--  Implementation of POSIX-like synchronisation objects under the
--  No_Tasking profile.

with PolyORB.Initialization;

with PolyORB.Utils.Strings;

package body PolyORB.Tasking.Profiles.No_Tasking.Condition_Variables is

   The_Condition : aliased No_Tasking_Condition_Type;

   ---------------
   -- Broadcast --
   ---------------

   procedure Broadcast
     (C : access No_Tasking_Condition_Type) is
      pragma Warnings (Off);
      pragma Unreferenced (C);
      pragma Warnings (On);
   begin
      null;
   end Broadcast;

   ------------
   -- Create --
   ------------

   function Create
     (MF   : access No_Tasking_Condition_Factory_Type;
      Name : String := "")
     return PTCV.Condition_Access is
      pragma Warnings (Off);
      pragma Unreferenced (MF);
      pragma Unreferenced (Name);
      pragma Warnings (On);
   begin
      return The_Condition'Access;
   end Create;

   -------------
   -- Destroy --
   -------------

   procedure Destroy
     (MF : access No_Tasking_Condition_Factory_Type;
      C  : in out PTCV.Condition_Access)
  is
      pragma Warnings (Off);
      pragma Unreferenced (MF);
      pragma Warnings (On);
   begin
      C := null;
   end Destroy;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is
   begin
      PTCV.Register_Condition_Factory (PTCV.Condition_Factory_Access
                                    (The_Condition_Factory));
   end Initialize;

   ------------
   -- Signal --
   ------------

   procedure Signal
     (C : access No_Tasking_Condition_Type) is
      pragma Warnings (Off);
      pragma Unreferenced (C);
      pragma Warnings (On);
   begin
      null;
   end Signal;

   ----------
   -- Wait --
   ----------

   procedure Wait
     (C : access No_Tasking_Condition_Type;
      M : access PTM.Mutex_Type'Class)
   is
      pragma Warnings (Off);
      pragma Unreferenced (M);
      pragma Unreferenced (C);
      pragma Warnings (On);
   begin
      raise Tasking_Error;
   end Wait;

   use PolyORB.Initialization;
   use PolyORB.Initialization.String_Lists;
   use PolyORB.Utils.Strings;

begin
   Register_Module
     (Module_Info'
      (Name      => +"tasking.profiles.no_tasking.condition_variables",
       Conflicts => Empty,
       Depends   => Empty,
       Provides  => +"tasking.condition_variables",
       Implicit  => False,
       Init      => Initialize'Access,
       Shutdown  => null));
end PolyORB.Tasking.Profiles.No_Tasking.Condition_Variables;
