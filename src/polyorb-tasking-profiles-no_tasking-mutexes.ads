------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--             P O L Y O R B . T A S K I N G . P R O F I L E S              --
--                  . N O _ T A S K I N G . M U T E X E S                   --
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

   procedure Enter (M : access No_Tasking_Mutex_Type);
   procedure Leave (M : access No_Tasking_Mutex_Type);

   type No_Tasking_Mutex_Factory_Type is
     new PTM.Mutex_Factory_Type with private;
   --  This type is a factory for the Mutex type under No_Tasking profile.

   type No_Tasking_Mutex_Factory_Access is
     access all No_Tasking_Mutex_Factory_Type'Class;

   The_Mutex_Factory : constant No_Tasking_Mutex_Factory_Access;

   function Create
     (MF   : access No_Tasking_Mutex_Factory_Type;
      Name : String := "")
     return PTM.Mutex_Access;
   --  Create a new mutex, or get a preallocated one.
   --  Name will be used to get the configuration of this
   --  Mutex from the configuration module.

   procedure Destroy
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
