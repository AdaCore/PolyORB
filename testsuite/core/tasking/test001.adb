------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                              T E S T 0 0 1                               --
--                                                                          --
--                                 B o d y                                  --
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

--  Thread testsuite.

--  $Id$

with Ada.Command_Line;
with Ada.Text_IO;

with PolyORB.Initialization;

with Test001_Common;

procedure Test001 is
   use Ada.Command_Line;
   use Ada.Text_IO;

   use Test001_Common;

   Nb_Of_Tasks : Natural := 1000;
begin
   if Ada.Command_Line.Argument_Count = 1 then
      Nb_Of_Tasks := Natural'Value (Ada.Command_Line.Argument (1));
   end if;
   Put_Line ("Generate test with" & Natural'Image (Nb_Of_Tasks) & " tasks");
   PolyORB.Initialization.Initialize_World;
   Initialize_Test;
   Test_Task_Creation (Nb_Of_Tasks);

end Test001;
