------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--        POLYORB.TASKING.PROFILES.FULL_TASKING.CONDITION_VARIABLES         --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--         Copyright (C) 2002-2005 Free Software Foundation, Inc.           --
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

--  Implementation of POSIX-like condition variables with full Ada
--  tasking. More comments can be found at polyorb-tasking-condition_variables.

with PolyORB.Tasking.Condition_Variables;
with PolyORB.Tasking.Mutexes;

package PolyORB.Tasking.Profiles.Full_Tasking.Condition_Variables is

   package PTM renames PolyORB.Tasking.Mutexes;

   package PTCV renames PolyORB.Tasking.Condition_Variables;

   type Full_Tasking_Condition_Type is
     new PTCV.Condition_Type with private;

   type Full_Tasking_Condition_Access is
     access all Full_Tasking_Condition_Type'Class;

   procedure Wait
     (Cond : access Full_Tasking_Condition_Type;
      M    : access PTM.Mutex_Type'Class);

   procedure Signal (Cond : access Full_Tasking_Condition_Type);

   procedure Broadcast (Cond : access Full_Tasking_Condition_Type);

   type Full_Tasking_Condition_Factory_Type is
     new PTCV.Condition_Factory_Type with private;

   type Full_Tasking_Condition_Factory_Access is
     access all Full_Tasking_Condition_Factory_Type'Class;

   The_Condition_Factory : constant Full_Tasking_Condition_Factory_Access;

   function Create
     (MF   : access Full_Tasking_Condition_Factory_Type;
      Name : String := "")
     return PTCV.Condition_Access;

   procedure Destroy
     (MF   : access Full_Tasking_Condition_Factory_Type;
      Cond : in out PTCV.Condition_Access);

private

   type Condition_PO;
   type Condition_PO_Access is access Condition_PO;

   type Full_Tasking_Condition_Type is new PTCV.Condition_Type with record
      The_PO : Condition_PO_Access;
   end record;

   type Full_Tasking_Condition_Factory_Type is
     new PTCV.Condition_Factory_Type with null record;

   The_Condition_Factory : constant Full_Tasking_Condition_Factory_Access
     := new Full_Tasking_Condition_Factory_Type;

end PolyORB.Tasking.Profiles.Full_Tasking.Condition_Variables;
