------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--         POLYORB.TASKING.PROFILES.NO_TASKING.CONDITION_VARIABLES          --
--                                                                          --
--                                 S p e c                                  --
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

--  Implementation of POSIX-like condition variables with no Ada tasking.

with PolyORB.Tasking.Condition_Variables;
with PolyORB.Tasking.Mutexes;

package PolyORB.Tasking.Profiles.No_Tasking.Condition_Variables is

   package PTCV renames PolyORB.Tasking.Condition_Variables;

   package PTM renames PolyORB.Tasking.Mutexes;

   procedure Initialize;
   --  Initialize this package

   type No_Tasking_Condition_Type
      is new PTCV.Condition_Type with private;

   type No_Tasking_Condition_Access is
     access all No_Tasking_Condition_Type'Class;
   --  Type for mutexes with no Ada tasking.

   overriding procedure Wait
     (C : access No_Tasking_Condition_Type;
      M : access PTM.Mutex_Type'Class);
   --  Have no sense in this profile (would block the only task that can run),
   --  so raise Tasking_Error.

   overriding procedure Signal
     (C : access No_Tasking_Condition_Type);

   overriding procedure Broadcast
     (C : access No_Tasking_Condition_Type);

   type No_Tasking_Condition_Factory_Type is
     new PTCV.Condition_Factory_Type with private;
   --  This type is a factory for the Condition type under No_Tasking profile.

   type No_Tasking_Condition_Factory_Access is
     access all No_Tasking_Condition_Factory_Type'Class;

   The_Condition_Factory : constant No_Tasking_Condition_Factory_Access;

   overriding function Create
     (MF   : access No_Tasking_Condition_Factory_Type;
      Name : String := "")
     return PTCV.Condition_Access;
   --  Create a new condition, or get a preallocated one.
   --  Name will be used to get the configuration of this
   --  condition from the configuration module.

   overriding procedure Destroy
     (MF : access No_Tasking_Condition_Factory_Type;
      C  : in out PTCV.Condition_Access);
   --  Destroy C.

private

   type No_Tasking_Condition_Type is new PTCV.Condition_Type with null record;

   type No_Tasking_Condition_Factory_Type is
     new PTCV.Condition_Factory_Type with null record;

   The_Condition_Factory : constant No_Tasking_Condition_Factory_Access
     := new No_Tasking_Condition_Factory_Type;

end PolyORB.Tasking.Profiles.No_Tasking.Condition_Variables;
