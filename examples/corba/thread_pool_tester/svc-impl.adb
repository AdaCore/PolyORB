------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                             S V C . I M P L                              --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--           Copyright (C) 2008, Free Software Foundation, Inc.             --
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

with Svc.Skel;
pragma Unreferenced (Svc.Skel);

with Ada.Text_IO; use Ada.Text_IO;

with Conditions; use Conditions;

with PolyORB.Tasking.Condition_Variables;
use  PolyORB.Tasking.Condition_Variables;

with PolyORB.Tasking.Mutexes;
use  PolyORB.Tasking.Mutexes;

package body Svc.Impl is

   ----------
   -- Wait --
   ----------

   procedure Wait
     (Self : not null access Object;
      Cond_Id : CORBA.Short)
   is
      pragma Unreferenced (Self);
   begin
      Put_Line ("Wait: enter, Cond_Id =" & Cond_Id'Img);
      Enter (Mutex);
      Wait (Condition_Variables (Natural (Cond_Id)), Mutex);
      Leave (Mutex);
      Put_Line ("Wait: leave, Cond_Id =" & Cond_Id'Img);
   end Wait;

end Svc.Impl;
