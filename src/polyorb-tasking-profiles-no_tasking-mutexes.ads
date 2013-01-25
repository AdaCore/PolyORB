------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--               POLYORB.TASKING.PROFILES.NO_TASKING.MUTEXES                --
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

pragma Ada_2005;

--  Implementation of POSIX-like mutexes with no Ada tasking.
--  For more information see PolyORB.Tasking.Mutexes.

with PolyORB.Tasking.Mutexes;

package PolyORB.Tasking.Profiles.No_Tasking.Mutexes is

   package PTM renames PolyORB.Tasking.Mutexes;

   procedure Initialize;
   --  Initialize this package

   type No_Tasking_Mutex_Type
      is new PTM.Mutex_Type with private;

   type No_Tasking_Mutex_Access is
     access all No_Tasking_Mutex_Type'Class;
   --  Type for mutexes with no Ada tasking.

   overriding procedure Enter (M : access No_Tasking_Mutex_Type);
   overriding procedure Leave (M : access No_Tasking_Mutex_Type);

   type No_Tasking_Mutex_Factory_Type is
     new PTM.Mutex_Factory_Type with private;
   --  This type is a factory for the Mutex type under No_Tasking profile.

   type No_Tasking_Mutex_Factory_Access is
     access all No_Tasking_Mutex_Factory_Type'Class;

   The_Mutex_Factory : constant No_Tasking_Mutex_Factory_Access;

   overriding function Create
     (MF   : access No_Tasking_Mutex_Factory_Type;
      Name : String := "")
     return PTM.Mutex_Access;
   --  Create a new mutex, or get a preallocated one.
   --  Name will be used to get the configuration of this
   --  Mutex from the configuration module.

   overriding procedure Destroy
     (MF : access No_Tasking_Mutex_Factory_Type;
      M  : in out PTM.Mutex_Access);
   --  Destroy M.

private

   type No_Tasking_Mutex_Type is new PTM.Mutex_Type with null record;

   type No_Tasking_Mutex_Factory_Type is
     new PTM.Mutex_Factory_Type with null record;

   The_Mutex_Factory : constant No_Tasking_Mutex_Factory_Access
     := new No_Tasking_Mutex_Factory_Type;

end PolyORB.Tasking.Profiles.No_Tasking.Mutexes;
