------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--               POLYORB.TASKING.PROFILES.RAVENSCAR.MUTEXES                 --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--         Copyright (C) 2002-2006, Free Software Foundation, Inc.          --
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
