------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--               POLYORB.TASKING.PROFILES.RAVENSCAR.MUTEXES                 --
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

--  Implementation of POSIX-like mutexes under the Ravenscar
--  profile. For more details see PolyORB.Tasking.Mutexes.

with PolyORB.Initialization;

with PolyORB.Tasking.Mutexes;
with PolyORB.Tasking.Profiles.Ravenscar.Index_Manager;
with PolyORB.Tasking.Profiles.Ravenscar.Threads;

generic
   with package Threads_For_Mutexes is
     new PolyORB.Tasking.Profiles.Ravenscar.Threads (<>);
   Number_Of_Mutexes : Integer;
package PolyORB.Tasking.Profiles.Ravenscar.Mutexes is

   use PolyORB.Tasking.Mutexes;

   type Ravenscar_Mutex_Type is new Mutex_Type with private;

   type Ravenscar_Mutex_Access is access all Ravenscar_Mutex_Type'Class;

   procedure Enter (M : access Ravenscar_Mutex_Type);
   procedure Leave (M : access Ravenscar_Mutex_Type);

   type Ravenscar_Mutex_Factory_Type is
     new Mutex_Factory_Type with private;

   type Ravenscar_Mutex_Factory_Access is
     access all Ravenscar_Mutex_Factory_Type'Class;

   The_Mutex_Factory : constant Ravenscar_Mutex_Factory_Access;

   function Create
     (MF   : access Ravenscar_Mutex_Factory_Type;
      Name : String := "")
     return Mutex_Access;

   procedure Destroy
     (MF : access Ravenscar_Mutex_Factory_Type;
      M  : in out Mutex_Access);

private
   use Threads_For_Mutexes;

   subtype Extended_Synchro_Index is Integer
     range Integer (Synchro_Index_Type'First) - 1 ..
     Integer (Synchro_Index_Type'Last);

   Null_Synchro_Index : constant Extended_Synchro_Index
     := Extended_Synchro_Index'First;

   package Mutex_Index_Manager is
      new PolyORB.Tasking.Profiles.Ravenscar.Index_Manager
     (Number_Of_Mutexes);

   subtype Mutex_Index_Type is Mutex_Index_Manager.Index_Type;

   type Ravenscar_Mutex_Type is new Mutex_Type with record
      Id : Mutex_Index_Type;
      --  Rank of the protected object used by this mutex
      --  in the preallocated array.
   end record;

   type Ravenscar_Mutex_Factory_Type is
     new Mutex_Factory_Type with null record;

   The_Mutex_Factory : constant Ravenscar_Mutex_Factory_Access
     := new Ravenscar_Mutex_Factory_Type;

   procedure Initialize;
   --  Initialize the package

   Initializer : constant PolyORB.Initialization.Initializer :=
                   Initialize'Access;

end PolyORB.Tasking.Profiles.Ravenscar.Mutexes;
