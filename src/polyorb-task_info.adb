------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                    P O L Y O R B . T A S K _ I N F O                     --
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

--  Information about running ORB tasks.
--  This packages is used to store and retrieve information
--  concerning the status of tasks that execute ORB functions.

--  $Id$

package body PolyORB.Task_Info is

   procedure Set_Status_Blocked
     (TI       : in out Task_Info;
      Selector : Asynch_Ev.Asynch_Ev_Monitor_Access) is
   begin
      pragma Assert (TI.Status = Running);

      TI.Status   := Blocked;
      TI.Selector := Selector;
   end Set_Status_Blocked;

   procedure Set_Status_Idle
     (TI      : in out Task_Info;
      Watcher : Soft_Links.Watcher_Access) is
   begin
      pragma Assert (TI.Status = Running);
      TI.Status  := Idle;
      TI.Watcher := Watcher;
   end Set_Status_Idle;

   procedure Set_Status_Running
     (TI : in out Task_Info) is
   begin
      pragma Assert (TI.Status /= Running);
      TI.Status   := Running;
      TI.Selector := null;
      TI.Watcher  := null;
   end Set_Status_Running;

   function Status (TI : Task_Info)
     return Task_Status is
   begin
      return TI.Status;
   end Status;

   function Selector (TI : Task_Info)
     return Asynch_Ev.Asynch_Ev_Monitor_Access is
   begin
      pragma Assert (TI.Status = Blocked);
      return TI.Selector;
   end Selector;

   function Watcher (TI : Task_Info)
     return Soft_Links.Watcher_Access is
   begin
      pragma Assert (TI.Status = Idle);
      return TI.Watcher;
   end Watcher;

end PolyORB.Task_Info;
