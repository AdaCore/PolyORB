--  Information about running ORB tasks.
--  This packages is used to store and retrieve information
--  concerning the status of tasks that execute ORB functions.

--  $Id$

package body Droopi.ORB.Task_Info is

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

end Droopi.ORB.Task_Info;
