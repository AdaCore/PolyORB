--  Information about running ORB tasks.
--  This packages is used to store and retrieve information
--  concerning the status of tasks that execute ORB functions.

--  $Id$

with GNAT.HTable;

package body Droopi.Task_Info is

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

   --------------------------------------------
   -- Task_Info global dictionnary accessors --
   --------------------------------------------

   subtype TI_Hash_Space is Integer range 0 .. 36;

   function TI_Hash_Func (TI : Integer) return TI_Hash_Space;
   --  A hash function over task ids.

   function TI_Hash_Func (TI : Integer) return TI_Hash_Space is
   begin
      return TI_Hash_Space'First +
        TI mod (1 + TI_Hash_Space'Last - TI_Hash_Space'First);
   end TI_Hash_Func;

   package Task_Info_Dict is new GNAT.HTable.Simple_HTable
     (Header_Num => TI_Hash_Space,
      Element    => Task_Info_Access,
      No_Element => null,
      Key        => Integer,
      Hash       => TI_Hash_Func,
      Equal      => "=");

   procedure Set_Task_Info
     (I : Task_Info_Access;
      T : Task_Id'Class := Current_Task) is
   begin
      Task_Info_Dict.Set (To_Integer (T), I);
   end Set_Task_Info;

   function Get_Task_Info
     (T : Task_Id'Class := Current_Task)
     return Task_Info_Access is
   begin
      return Task_Info_Dict.Get (To_Integer (T));
   end Get_Task_Info;

   procedure Reset_Task_Info
     (T : Task_Id'Class := Current_Task) is
   begin
      Task_Info_Dict.Remove (To_Integer (T));
   end Reset_Task_Info;

end Droopi.Task_Info;
