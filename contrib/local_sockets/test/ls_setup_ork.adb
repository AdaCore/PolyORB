------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                         L S _ S E T U P _ O R K                          --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--            Copyright (C) 2005 Free Software Foundation, Inc.             --
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
--                  PolyORB is maintained by AdaCore                        --
--                     (email: sales@adacore.com)                           --
--                                                                          --
------------------------------------------------------------------------------

with Kernel.Serial_Output; use Kernel.Serial_Output;
with Kernel.Peripherals; use Kernel.Peripherals;

with Ravenscar_Setup;
pragma Warnings (Off, Ravenscar_Setup);
pragma Elaborate_All (Ravenscar_Setup);

with PolyORB.Log.ORK_Serial;
pragma Warnings (Off, PolyORB.Log.ORK_Serial);
pragma Elaborate_All (PolyORB.Log.ORK_Serial);

with PolyORB.ORB.Thread_Pool;
pragma Warnings (Off, PolyORB.ORB.Thread_Pool);
pragma Elaborate_All (PolyORB.ORB.Thread_Pool);

with PolyORB.ORB_Controller.Workers;
pragma Warnings (Off, PolyORB.ORB_Controller.Workers);
pragma Elaborate_All (PolyORB.ORB_Controller.Workers);

with PolyORB.Setup.OA.Basic_POA;
pragma Warnings (Off, PolyORB.Setup.OA.Basic_POA);
pragma Elaborate_All (PolyORB.Setup.OA.Basic_POA);

with PolyORB.Setup.OA.Basic_POA;
pragma Warnings (Off, PolyORB.Setup.OA.Basic_POA);
pragma Elaborate_All (PolyORB.Setup.OA.Basic_POA);

with PolyORB.Setup.LS;
pragma Elaborate_All (PolyORB.Setup.LS);
pragma Warnings (Off, PolyORB.Setup.LS);

with PolyORB.Setup.Access_Points.LS;
pragma Elaborate_All (PolyORB.Setup.Access_Points.LS);
pragma Warnings (Off, PolyORB.Setup.Access_Points.LS);

with PolyORB.Binding_Data.GIOP.Local_Sockets;
pragma Elaborate_All (PolyORB.Binding_Data.GIOP.Local_Sockets);
pragma Warnings (Off, PolyORB.Binding_Data.GIOP.Local_Sockets);


with PolyORB.Binding_Data.GIOP;
pragma Elaborate_All (PolyORB.Binding_Data.GIOP);
pragma Warnings (Off, PolyORB.Binding_Data.GIOP);

package body LS_Setup_ORK is
begin
   Kernel.Serial_Output.Init_Serial_Line
     (Kernel.Peripherals.Serial_Port_1);

end LS_Setup_ORK;
