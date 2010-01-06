------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--     P O L Y O R B . S E T U P . T H R E A D _ P O O L _ C L I E N T      --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--           Copyright (C) 2009, Free Software Foundation, Inc.             --
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

with PolyORB.Setup.Client_Base;
pragma Warnings (Off, PolyORB.Setup.Client_Base);
pragma Elaborate_All (PolyORB.Setup.Client_Base);

with PolyORB.ORB.Thread_Pool;
pragma Elaborate_All (PolyORB.ORB.Thread_Pool);
pragma Warnings (Off, PolyORB.ORB.Thread_Pool);

with PolyORB.Setup.Tasking.Full_Tasking;
pragma Elaborate_All (PolyORB.Setup.Tasking.Full_Tasking);
pragma Warnings (Off, PolyORB.Setup.Tasking.Full_Tasking);

with PolyORB.ORB_Controller.Workers;
pragma Warnings (Off, PolyORB.ORB_Controller.Workers);
pragma Elaborate_All (PolyORB.ORB_Controller.Workers);

package body PolyORB.Setup.Thread_Pool_Client is
end PolyORB.Setup.Thread_Pool_Client;
