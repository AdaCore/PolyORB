------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--           P O L Y O R B . R A V E N S C A R . M O N I T O R S            --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--             Copyright (C) 1999-2002 Free Software Fundation              --
--                                                                          --
-- PolyORB is free software; you  can  redistribute  it and/or modify it    --
-- under terms of the  GNU General Public License as published by the  Free --
-- Software Foundation;  either version 2,  or (at your option)  any  later --
-- version. PolyORB is distributed  in the hope that it will be  useful,    --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License  for more details.  You should have received  a copy of the GNU  --
-- General Public License distributed with PolyORB; see file COPYING. If    --
-- not, write to the Free Software Foundation, 59 Temple Place - Suite 330, --
-- Boston, MA 02111-1307, USA.                                              --
--                                                                          --
-- As a special exception,  if other files  instantiate  generics from this --
-- unit, or you link  this unit with other files  to produce an executable, --
-- this  unit  does not  by itself cause  the resulting  executable  to  be --
-- covered  by the  GNU  General  Public  License.  This exception does not --
-- however invalidate  any other reasons why  the executable file  might be --
-- covered by the  GNU Public License.                                      --
--                                                                          --
--              PolyORB is maintained by ENST Paris University.             --
--                                                                          --
------------------------------------------------------------------------------

--  Implementation of Monitors under the Ravenscar profile. For more comments
--  see PolyORB.Tasking.Monitors.

--  $Id$

with PolyORB.Tasking.Monitors;
with PolyORB.Ravenscar.Configuration;
with PolyORB.Ravenscar.Index_Manager;

package PolyORB.Ravenscar.Monitors is
   use PolyORB.Tasking.Monitors;

   type Ravenscar_Monitor_Type is
     new Monitor_Type with private;
   --  Monitor type for the Ravenscar profile.

   type Ravenscar_Monitor_Access is access all Ravenscar_Monitor_Type'Class;

   procedure Enter (M : in out Ravenscar_Monitor_Type);
   --  Enter critical section.

   procedure Leave (M : in out Ravenscar_Monitor_Type);
   --  Leave critical section.

   procedure Wait
     (M : in out Ravenscar_Monitor_Type;
      C : access Condition_Type'Class);
   --  Wait until the "C" condition is set to True.
   --  Free the monitor during this time.

   procedure Signal
     (M : in out Ravenscar_Monitor_Type);

   type Ravenscar_Monitor_Factory_Type is
     new Monitor_Factory_Type with private;
   --  Monitor factory type for the Ravenscar profile.

   type Ravenscar_Monitor_Factory_Access is
     access all Ravenscar_Monitor_Factory_Type'Class;

   The_Monitor_Factory : constant Ravenscar_Monitor_Factory_Access;
   --  The Monitor factory for teh Ravenscar profile.

   function Create
     (MF   : access Ravenscar_Monitor_Factory_Type;
      Name : String := "")
     return Monitor_Access;
   --  Create a new monitor, or get a preallocated one.
   --  Name will be used to get the configuration of this
   --  Monitor from the configuration module.

   procedure Destroy
     (MF : in out Ravenscar_Monitor_Factory_Type;
      M  : in out Monitor_Access);
   --  Destroy M, or just release it if it was preallocated.

   procedure Initialize;
   --  Initialize the package.

private
   use PolyORB.Ravenscar.Configuration;

   subtype Extended_Thread_Index is
     Integer range -1 .. Number_Of_Threads - 1;

   Null_Thread_Index : constant Extended_Thread_Index := -1;

   subtype Thread_Index is
     Extended_Thread_Index range 0 .. Number_Of_Threads - 1;

   package My_Index_Manager is
      new PolyORB.Ravenscar.Index_Manager (Configuration.Number_Of_Monitors);

   subtype Monitor_Index_Type is My_Index_Manager.Index_Type;

   type Ravenscar_Monitor_Type is new Monitor_Type with record
      Id : Monitor_Index_Type;
      --  Rank of the protected object used by this monitor
      --  in the preallocated array.
   end record;

   type Ravenscar_Monitor_Factory_Type is
     new Monitor_Factory_Type with null record;

   The_Monitor_Factory : constant Ravenscar_Monitor_Factory_Access
     := new Ravenscar_Monitor_Factory_Type;

end PolyORB.Ravenscar.Monitors;
