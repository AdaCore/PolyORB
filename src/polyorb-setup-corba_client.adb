------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--           P O L Y O R B . S E T U P . C O R B A _ C L I E N T            --
--                                                                          --
--                                 B o d y                                  --
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

--  Set up a test ORB.

--  $Id$

with PolyORB.Initialization;

with PolyORB.ORB;
with PolyORB.ORB.No_Tasking;
with PolyORB.Binding_Data.IIOP;
with PolyORB.Binding_Data.SOAP;

pragma Elaborate_All (PolyORB.ORB);
pragma Elaborate_All (PolyORB.ORB.No_Tasking);
pragma Elaborate_All (PolyORB.Binding_Data.IIOP);
pragma Elaborate_All (PolyORB.Binding_Data.SOAP);

pragma Warnings (Off, PolyORB.ORB);
pragma Warnings (Off, PolyORB.ORB.No_Tasking);
pragma Warnings (Off, PolyORB.Binding_Data.IIOP);
pragma Warnings (Off, PolyORB.Binding_Data.SOAP);

package body PolyORB.Setup.CORBA_Client is

begin
   PolyORB.Initialization.Initialize_World;
end PolyORB.Setup.CORBA_Client;
