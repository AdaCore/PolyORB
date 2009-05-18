------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--              POLYORB.TASKING.PROFILES.FULL_TASKING.MUTEXES               --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--         Copyright (C) 2002-2009, Free Software Foundation, Inc.          --
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

pragma Warnings (Off);
--  Depends on System.Task_Primitives, an internal GNAT unit
with System.Task_Primitives;
pragma Warnings (On);

with PolyORB.Tasking.Mutexes;

package PolyORB.Tasking.Profiles.Full_Tasking.Mutexes is

   package PTM renames PolyORB.Tasking.Mutexes;

   type Full_Tasking_Mutex_Type is
     new PTM.Mutex_Type with private;

   type Full_Tasking_Mutex_Access is
     access all Full_Tasking_Mutex_Type'Class;

   procedure Enter (M : access Full_Tasking_Mutex_Type);
   pragma Inline (Enter);

   procedure Leave (M : access Full_Tasking_Mutex_Type);
   pragma Inline (Leave);

   type Full_Tasking_Mutex_Factory_Type is
     new PTM.Mutex_Factory_Type with private;

   type Full_Tasking_Mutex_Factory_Access is
     access all Full_Tasking_Mutex_Factory_Type'Class;

   The_Mutex_Factory : constant Full_Tasking_Mutex_Factory_Access;

   function Create
     (MF   : access Full_Tasking_Mutex_Factory_Type;
      Name : String := "")
     return PTM.Mutex_Access;

   procedure Destroy
     (MF : access Full_Tasking_Mutex_Factory_Type;
      M  : in out PTM.Mutex_Access);

private

   subtype Mutex_Lock is System.Task_Primitives.Lock;
   type Mutex_Lock_Access is access Mutex_Lock;

   type Full_Tasking_Mutex_Type is new PTM.Mutex_Type with record
      The_Lock : Mutex_Lock_Access;
   end record;

   type Full_Tasking_Mutex_Factory_Type is
     new PTM.Mutex_Factory_Type with null record;

   The_Mutex_Factory : constant Full_Tasking_Mutex_Factory_Access
     := new Full_Tasking_Mutex_Factory_Type;

end PolyORB.Tasking.Profiles.Full_Tasking.Mutexes;
