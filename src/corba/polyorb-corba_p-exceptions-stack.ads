------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--     P O L Y O R B . C O R B A _ P . E X C E P T I O N S . S T A C K      --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                Copyright (C) 2001 Free Software Fundation                --
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

--  $Id: //droopi/main/src/corba/polyorb-corba_p-exceptions-stack.ads#2 $

with CORBA;

package PolyORB.CORBA_P.Exceptions.Stack is

   pragma Elaborate_Body;

   procedure Get_Members
     (Exc_Occ : in CORBA.Exception_Occurrence;
      Exc_Mbr : out IDL_Exception_Members'Class);
   --  Find the members object associated to a given exception occurrence.
   --  Remove it from the stack and return it. Raise CORBA.Imp_Limit if
   --  the members are not in this stack.

   procedure Purge_Members
     (Exc_Occ : in CORBA.Exception_Occurrence);
   --  Remove the members object associated to a given exception occurrence.
   --  No exception is raised.

   procedure Raise_Exception
     (Exc_Id  : in Ada.Exceptions.Exception_Id;
      Exc_Mbr : in IDL_Exception_Members'Class);
   pragma No_Return (Raise_Exception);
   --  Raise a CORBA exception associated to the member object
   --  Exc_Mbr. Store its members in a stack. If the stack size is
   --  bigger than stack_size, the oldest members are thrown away.

end PolyORB.CORBA_P.Exceptions.Stack;








