--                 POLYORB.SETUP.THREAD_PER_REQUEST_SERVER                  --
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

--  Elaborate a complete server with the ``thread-per-request''
--  tasking policy.


with PolyORB.Initialization;
with PolyORB.ORB.Thread_Per_Request;
with PolyORB.Profiles.Full_Tasking;
with PolyORB.Tasking.Soft_Links;
with PolyORB.Setup.Server;

pragma Elaborate_All (PolyORB.ORB.Thread_Per_Request);
pragma Elaborate_All (PolyORB.Profiles.Full_Tasking);
pragma Elaborate_All (PolyORB.Tasking.Soft_Links);
pragma Elaborate_All (PolyORB.Setup.Server);

pragma Warnings (Off, PolyORB.Initialization);
pragma Warnings (Off, PolyORB.ORB.Thread_Per_Request);
pragma Warnings (Off, PolyORB.Profiles.Full_Tasking);
pragma Warnings (Off, PolyORB.Tasking.Soft_Links);
pragma Warnings (Off, PolyORB.Setup.Server);

package body PolyORB.Setup.Thread_Per_Request_Server is

end PolyORB.Setup.Thread_Per_Request_Server;
