------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--             P O L Y O R B . T A S K I N G . P R O F I L E S              --
--                . F U L L _ T A S K I N G . M U T E X E S                 --
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

--  Implementation of POSIX-like mutexes with full Ada
--  tasking. More comments can be found at polyorb-tasking-mutexes

with PolyORB.Tasking.Mutexes;

package PolyORB.Tasking.Profiles.Full_Tasking.Mutexes is

   package PTM renames PolyORB.Tasking.Mutexes;

   type Full_Tasking_Mutex_Type is
     new PTM.Mutex_Type with private;

   type Full_Tasking_Mutex_Access is
     access all Full_Tasking_Mutex_Type'Class;

   procedure Enter (M : in out Full_Tasking_Mutex_Type);

   procedure Leave (M : in out Full_Tasking_Mutex_Type);

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
     (MF : in out Full_Tasking_Mutex_Factory_Type;
      M  : in out PTM.Mutex_Access);

private

   type Mutex_PO;
   type Mutex_PO_Access is access Mutex_PO;

   type Full_Tasking_Mutex_Type is new PTM.Mutex_Type with record
      The_PO : Mutex_PO_Access;
   end record;

   type Full_Tasking_Mutex_Factory_Type is
     new PTM.Mutex_Factory_Type with null record;

   The_Mutex_Factory : constant Full_Tasking_Mutex_Factory_Access
     := new Full_Tasking_Mutex_Factory_Type;

end PolyORB.Tasking.Profiles.Full_Tasking.Mutexes;
