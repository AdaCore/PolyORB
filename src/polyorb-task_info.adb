------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                    P O L Y O R B . T A S K _ I N F O                     --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2001-2003 Free Software Foundation, Inc.           --
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
--                PolyORB is maintained by ACT Europe.                      --
--                    (email: sales@act-europe.fr)                          --
--                                                                          --
------------------------------------------------------------------------------

--  Information about running ORB tasks.
--  This packages is used to store and retrieve information
--  concerning the status of tasks that execute ORB functions.

--  $Id$

package body PolyORB.Task_Info is

   -----------
   -- Image --
   -----------

   function Image (TI : Task_Info) return String is
   begin
      return Tasking.Threads.Image (TI.Id);
   end Image;

   --------------
   -- Selector --
   --------------

   function Selector (TI : Task_Info)
     return Asynch_Ev.Asynch_Ev_Monitor_Access is
   begin
      pragma Assert (TI.Status = Blocked);
      return TI.Selector;
   end Selector;

   ------------------------
   -- Set_Status_Blocked --
   ------------------------

   procedure Set_Status_Blocked
     (TI       : in out Task_Info;
      Selector : Asynch_Ev.Asynch_Ev_Monitor_Access) is
   begin
      pragma Assert (TI.Status = Running);

      TI.Status   := Blocked;
      TI.Selector := Selector;
   end Set_Status_Blocked;

   ---------------------
   -- Set_Status_Idle --
   ---------------------

   procedure Set_Status_Idle
     (TI        : in out Task_Info;
      Condition : Tasking.Condition_Variables.Condition_Access) is
   begin
      pragma Assert (TI.Status = Running);
      TI.Status    := Idle;
      TI.Condition := Condition;
   end Set_Status_Idle;

   ------------------------
   -- Set_Status_Running --
   ------------------------

   procedure Set_Status_Running
     (TI : in out Task_Info) is
   begin
      pragma Assert (TI.Status /= Running);
      TI.Status    := Running;
      TI.Selector  := null;
      TI.Condition := null;
   end Set_Status_Running;

   ------------
   -- Set_Id --
   ------------

   procedure Set_Id (TI : in out Task_Info) is
   begin
      TI.Id := Tasking.Threads.Current_Task;
   end Set_Id;

   ------------
   -- Status --
   ------------

   function Status (TI : Task_Info)
     return Task_Status is
   begin
      return TI.Status;
   end Status;

   ---------------
   -- Condition --
   ---------------

   function Condition (TI : Task_Info)
     return Tasking.Condition_Variables.Condition_Access is
   begin
      pragma Assert (TI.Status = Idle);
      return TI.Condition;
   end Condition;

end PolyORB.Task_Info;
