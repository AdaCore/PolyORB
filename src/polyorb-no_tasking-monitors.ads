------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--          P O L Y O R B - N O _ T A S K I N G - M O N I T O R S           --
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

--  Implementation of monitors with no Ada tasking.

--  $Id$

with PolyORB.Tasking.Monitors;

package PolyORB.No_Tasking.Monitors is

   package PTM renames PolyORB.Tasking.Monitors;

   procedure Initialize;
   --  Initialize this package

   type No_Tasking_Monitor_Type
      is new PTM.Monitor_Type with private;

   type No_Tasking_Monitor_Access is
     access all No_Tasking_Monitor_Type'Class;
   --  Type for monitors with no Ada tasking.

   procedure Enter (M : in out No_Tasking_Monitor_Type);
   --  Enter critical section.

   procedure Leave (M : in out No_Tasking_Monitor_Type);
   --  Leave critical section.

   procedure Wait
     (M : in out No_Tasking_Monitor_Type;
      C : access PTM.Condition_Type'Class);
   --  Wait until the C condition is set to True.
   --  Free the monitor during this time.

   procedure Signal
     (M : in out No_Tasking_Monitor_Type);

   type No_Tasking_Monitor_Factory_Type is
     new PTM.Monitor_Factory_Type with private;
   --  This type is a factory for the Monitor type under No_Tasking profile.

   type No_Tasking_Monitor_Factory_Access is
     access all No_Tasking_Monitor_Factory_Type'Class;

   The_Monitor_Factory : constant No_Tasking_Monitor_Factory_Access;

   function Create
     (MF   : access No_Tasking_Monitor_Factory_Type;
      Name : String := "")
     return PTM.Monitor_Access;
   --  Create a new monitor, or get a preallocated one.
   --  Name will be used to get the configuration of this
   --  Monitor from the configuration module.

   procedure Destroy
     (MF : in out No_Tasking_Monitor_Factory_Type;
      M  : in out PTM.Monitor_Access);
   --  Destroy M.

private

   type No_Tasking_Monitor_Type is new PTM.Monitor_Type with null record;

   type No_Tasking_Monitor_Factory_Type is
     new PTM.Monitor_Factory_Type with null record;

   The_Monitor_Factory : constant No_Tasking_Monitor_Factory_Access
     := new No_Tasking_Monitor_Factory_Type;

end PolyORB.No_Tasking.Monitors;
