------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--         P O L Y O R B . T E S T . T H R E A D _ P O O L _ P O A          --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                Copyright (C) 2001 Free Software Fundation                --
--                                                                          --
-- AdaBroker is free software; you  can  redistribute  it and/or modify it  --
-- under terms of the  GNU General Public License as published by the  Free --
-- Software Foundation;  either version 2,  or (at your option)  any  later --
-- version. AdaBroker  is distributed  in the hope that it will be  useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License  for more details.  You should have received  a copy of the GNU  --
-- General Public License distributed with AdaBroker; see file COPYING. If  --
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

--  Set up a test server with the Thread_Pool tasking policy.

--  $Id$

with PolyORB.Setup.Test; use PolyORB.Setup.Test;
with PolyORB.Protected_Objects;
with PolyORB.ORB.Thread_Pool;
with PolyORB.Setup.Test_CORBA;

procedure PolyORB.Test.Thread_Pool_POA is
begin
   Initialize_Test_Server
     (PolyORB.Protected_Objects.Initialize'Access,
      new PolyORB.ORB.Thread_Pool.Thread_Pool_Policy);
   ORB.Thread_Pool.Initialize (4, 10);

   Initialize_Test_Access_Points;
   Setup.Test_CORBA.Initialize_CORBA_Test_Object;
   Run_Test;
end PolyORB.Test.Thread_Pool_POA;
