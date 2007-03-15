------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--         POLYORB.TASKING.PROFILES.RAVENSCAR.CONDITION_VARIABLES           --
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

--  Implementation of POSIX-like condition variables under the Ravenscar
--  profile. For more details see PolyORB.Tasking.Condition_Variables

with PolyORB.Initialization;

with PolyORB.Tasking.Condition_Variables;
with PolyORB.Tasking.Mutexes;
with PolyORB.Tasking.Profiles.Ravenscar.Index_Manager;
with PolyORB.Tasking.Profiles.Ravenscar.Threads;

generic
   with package Threads_For_CV is
     new PolyORB.Tasking.Profiles.Ravenscar.Threads (<>);
   Number_Of_Conditions : Integer;
package PolyORB.Tasking.Profiles.Ravenscar.Condition_Variables is

   use PolyORB.Tasking.Condition_Variables;
   use PolyORB.Tasking.Mutexes;

   type Ravenscar_Condition_Type is new Condition_Type with private;

   type Ravenscar_Condition_Access is
     access all Ravenscar_Condition_Type'Class;

   procedure Wait
     (Cond : access Ravenscar_Condition_Type;
      M    : access Mutex_Type'Class);

   procedure Signal (Cond : access Ravenscar_Condition_Type);

   procedure Broadcast (Cond : access Ravenscar_Condition_Type);

   type Ravenscar_Condition_Factory_Type is
     new Condition_Factory_Type with private;

   type Ravenscar_Condition_Factory_Access is
     access all Ravenscar_Condition_Factory_Type'Class;

   The_Condition_Factory : constant Ravenscar_Condition_Factory_Access;

   function Create
     (MF   : access Ravenscar_Condition_Factory_Type;
      Name : String := "")
     return Condition_Access;

   procedure Destroy
     (MF   : access Ravenscar_Condition_Factory_Type;
      Cond : in out Condition_Access);

private
   use Threads_For_CV;

   subtype Extended_Synchro_Index is Integer
     range Integer (Synchro_Index_Type'First) - 1 ..
     Integer (Synchro_Index_Type'Last);

   Null_Synchro_Index : constant Extended_Synchro_Index
     := Integer (Synchro_Index_Type'First) - 1;

   package Condition_Index_Manager is
      new PolyORB.Tasking.Profiles.Ravenscar.Index_Manager
     (Number_Of_Conditions);

   subtype Condition_Index_Type is Condition_Index_Manager.Index_Type;

   type Ravenscar_Condition_Type is new Condition_Type with record
      Id : Condition_Index_Type;
      --  Rank of the protected object used by this condition variable in the
      --  preallocated array.
   end record;

   type Ravenscar_Condition_Factory_Type is
     new Condition_Factory_Type with null record;

   The_Condition_Factory : constant Ravenscar_Condition_Factory_Access
     := new Ravenscar_Condition_Factory_Type;

   procedure Initialize;
   --  Initialize the package

   Initializer : constant PolyORB.Initialization.Initializer :=
                   Initialize'Access;

end PolyORB.Tasking.Profiles.Ravenscar.Condition_Variables;
