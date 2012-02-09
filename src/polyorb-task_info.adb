------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                    P O L Y O R B . T A S K _ I N F O                     --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2001-2012, Free Software Foundation, Inc.          --
--                                                                          --
-- This is free software;  you can redistribute it  and/or modify it  under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  This software is distributed in the hope  that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License for  more details.                                               --
--                                                                          --
-- As a special exception under Section 7 of GPL version 3, you are granted --
-- additional permissions described in the GCC Runtime Library Exception,   --
-- version 3.1, as published by the Free Software Foundation.               --
--                                                                          --
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
--                                                                          --
--                  PolyORB is maintained by AdaCore                        --
--                     (email: sales@adacore.com)                           --
--                                                                          --
------------------------------------------------------------------------------

pragma Ada_2005;

package body PolyORB.Task_Info is

   procedure Increment (C : in out Natural);
   procedure Decrement (C : in out Natural);
   --  Increment / decrement C

   procedure Task_State_Change
     (Summary   : in out Task_Summary;
      TI        : in out Task_Info;
      New_State : Task_State);
   --  Set TI's state to New_State and record transition in Summary

   ---------------
   -- Decrement --
   ---------------

   procedure Decrement (C : in out Natural) is
   begin
      C := C - 1;
   end Decrement;

   ---------------
   -- Increment --
   ---------------

   procedure Increment (C : in out Natural) is
   begin
      C := C + 1;
   end Increment;

   ---------------
   -- Get_Count --
   ---------------

   function Get_Count
      (Summary : Task_Summary;
       Kind    : Any_Task_Kind  := Any;
       State   : Any_Task_State := Any) return Natural
   is
   begin
      return Summary.Counters (Kind, State);
   end Get_Count;

   -----------
   -- Image --
   -----------

   function Image (TI : Task_Info) return String is
   begin
      return Tasking.Threads.Image (TI.Id);
   end Image;

   ----------------
   -- Kind_Match --
   ----------------

   function Kind_Match (TI : Task_Info; Kind : Any_Task_Kind) return Boolean is
   begin
      return Kind = Any or else Kind = TI.Kind;
   end Kind_Match;

   -----------------
   -- List_Attach --
   -----------------

   procedure List_Attach
     (TI   : access Task_Info;
      List : in out Task_List)
   is
      pragma Assert (not TI.On_List);
   begin
      Prepend (List, TI);
      TI.On_List := True;
   end List_Attach;

   -----------------
   -- List_Detach --
   -----------------

   procedure List_Detach
     (TI   : access Task_Info;
      List : in out Task_List)
   is
   begin
      if TI.On_List then
         Remove_Element (List, TI);
         TI.On_List := False;
      end if;
   end List_Detach;

   ----------------
   -- List_First --
   ----------------

   function List_First (List : Task_List) return access Task_Info is
   begin
      return Task_Lists.Value (First (List));
   end List_First;

   --------------
   -- May_Exit --
   --------------

   function May_Exit (TI : Task_Info) return Boolean is
   begin
      return TI.May_Exit;
   end May_Exit;

   -------------
   -- On_List --
   -------------

   function On_List (TI : Task_Info) return Boolean is
   begin
      return TI.On_List;
   end On_List;

   --------------
   -- Selector --
   --------------

   function Selector
     (TI : Task_Info)
     return Asynch_Ev.Asynch_Ev_Monitor_Access
   is
   begin
      pragma Assert (TI.State = Blocked);

      return TI.Selector;
   end Selector;

   -------------
   -- Timeout --
   -------------

   function Timeout (TI : Task_Info) return Duration is
   begin
      pragma Assert (TI.State = Blocked);

      return TI.Timeout;
   end Timeout;

   -----------------------
   -- Set_State_Blocked --
   -----------------------

   procedure Set_State_Blocked
     (Summary  : in out Task_Summary;
      TI       : in out Task_Info;
      Selector :        Asynch_Ev.Asynch_Ev_Monitor_Access;
      Timeout  :        Duration)
   is
   begin
      Task_State_Change (Summary, TI, New_State => Blocked);
      TI.Selector := Selector;
      TI.Timeout  := Timeout;
   end Set_State_Blocked;

   --------------------
   -- Set_State_Idle --
   --------------------

   procedure Set_State_Idle
     (Summary   : in out Task_Summary;
      TI        : in out Task_Info;
      Condition : PTCV.Condition_Access;
      Mutex     : PTM.Mutex_Access)
   is
   begin
      Task_State_Change (Summary, TI, New_State => Idle);
      TI.Condition := Condition;
      TI.Mutex     := Mutex;
   end Set_State_Idle;

   -----------------------
   -- Set_State_Running --
   -----------------------

   procedure Set_State_Running
     (Summary : in out Task_Summary;
      TI      : in out Task_Info;
      Job     : Jobs.Job_Access)
   is
   begin
      Task_State_Change (Summary, TI, New_State => Running);
      TI.Job       := Job;
      TI.Selector  := null;
      TI.Condition := null;
      TI.Mutex     := null;
   end Set_State_Running;

   -----------
   -- State --
   -----------

   function State (TI : Task_Info) return Task_State is
   begin
      return TI.State;
   end State;

   ---------------
   -- Condition --
   ---------------

   function Condition (TI : Task_Info) return PTCV.Condition_Access is
   begin
      return TI.Condition;
   end Condition;

   --------------------
   -- Exit_Condition --
   --------------------

   function Exit_Condition (TI : Task_Info) return Boolean is
      use type PolyORB.Types.Boolean_Ptr;

   begin
      return TI.Exit_Condition /= null and then TI.Exit_Condition.all;
   end Exit_Condition;

   ----------
   -- Link --
   ----------

   function Link
     (S     : access Task_Info;
      Which : Utils.Ilists.Link_Type) return access Task_Info_Access
   is
   begin
      return S.Links (Which)'Unchecked_Access;
   end Link;

   -----------
   -- Mutex --
   -----------

   function Mutex (TI : Task_Info) return PTM.Mutex_Access is
   begin
      return TI.Mutex;
   end Mutex;

   ------------------------
   -- Set_Exit_Condition --
   ------------------------

   procedure Set_Exit_Condition
     (TI             : in out Task_Info;
      Exit_Condition : Types.Boolean_Ptr)
   is
      use type Types.Boolean_Ptr;

   begin
      pragma Assert ((TI.Kind = Permanent) = (Exit_Condition = null));
      TI.Exit_Condition := Exit_Condition;
   end Set_Exit_Condition;

   ------------
   -- Set_Id --
   ------------

   procedure Set_Id (TI : in out Task_Info) is
   begin
      TI.Id := Tasking.Threads.Current_Task;
   end Set_Id;

   ------------------
   -- Set_May_Exit --
   ------------------

   procedure Set_May_Exit
     (TI       : in out Task_Info;
      May_Exit : Boolean)
   is
   begin
      TI.May_Exit := May_Exit;
   end Set_May_Exit;

   ---------------------------
   -- Set_State_Unscheduled --
   ---------------------------

   procedure Set_State_Unscheduled
     (Summary : in out Task_Summary;
      TI      : in out Task_Info)
   is
   begin
      --  Note: TI may already be in Unscheduled state, because this is the
      --  initial state of a newly-created task.

      Task_State_Change (Summary, TI, New_State => Unscheduled);
      TI.Job       := null;
      TI.Selector  := null;
      TI.Condition := null;
      TI.Mutex     := null;
   end Set_State_Unscheduled;

   --------------------------
   -- Set_State_Terminated --
   --------------------------

   procedure Set_State_Terminated
     (Summary : in out Task_Summary;
      TI      : in out Task_Info) is
   begin
      Task_State_Change (Summary, TI, Terminated);
   end Set_State_Terminated;

   ---------------------------
   -- Request_Abort_Polling --
   ---------------------------

   procedure Request_Abort_Polling (TI : in out Task_Info) is
   begin
      pragma Assert (TI.State = Blocked);

      TI.Abort_Polling := True;
   end Request_Abort_Polling;

   -------------------
   -- Abort_Polling --
   -------------------

   function Abort_Polling (TI : Task_Info) return Boolean is
   begin
      pragma Assert (TI.State = Blocked);

      return TI.Abort_Polling;
   end Abort_Polling;

   --------
   -- Id --
   --------

   function Id (TI : Task_Info) return PolyORB.Tasking.Threads.Thread_Id is
   begin
      return TI.Id;
   end Id;

   --------------
   -- Is_Empty --
   --------------

   overriding function Is_Empty (List : Task_List) return Boolean is
   begin
      return Task_Lists.Is_Empty (Task_Lists.List (List));
   end Is_Empty;

   ---------
   -- Job --
   ---------

   function Job (TI : Task_Info) return Jobs.Job_Access is
   begin
      return TI.Job;
   end Job;

   ------------------
   -- Task_Created --
   ------------------

   procedure Task_Created (Summary : in out Task_Summary; TI : Task_Info) is
   begin
      pragma Assert (TI.State = Unscheduled);
      Increment (Summary.Counters (TI.Kind, TI.State));
      Increment (Summary.Counters (TI.Kind, Any));
      Increment (Summary.Counters (Any,     TI.State));
      Increment (Summary.Counters (Any,     Any));
   end Task_Created;

   ------------------
   -- Task_Removed --
   ------------------

   procedure Task_Removed (Summary : in out Task_Summary; TI : Task_Info) is
   begin
      pragma Assert (TI.State = Terminated);
      Decrement (Summary.Counters (TI.Kind, TI.State));
      Decrement (Summary.Counters (TI.Kind, Any));
      Decrement (Summary.Counters (Any,     TI.State));
      Decrement (Summary.Counters (Any,     Any));
   end Task_Removed;

   -----------------------
   -- Task_State_Change --
   -----------------------

   procedure Task_State_Change
     (Summary   : in out Task_Summary;
      TI        : in out Task_Info;
      New_State : Task_State)
   is
   begin
      Decrement (Summary.Counters (TI.Kind, TI.State));
      Decrement (Summary.Counters (Any,     TI.State));
      TI.State := New_State;
      Increment (Summary.Counters (TI.Kind, TI.State));
      Increment (Summary.Counters (Any,     TI.State));
      pragma Assert (Task_Summary_Valid (Summary));
   end Task_State_Change;

   ------------------------
   -- Task_Summary_Valid --
   ------------------------

   function Task_Summary_Valid (Summary : Task_Summary) return Boolean
   is
      Count       : Natural;
      Total_Count : Natural;
   begin
      --  Check per-kind summary

      for K in Task_Kind'Range loop
         Count := 0;
         for S in Task_State'Range loop
            Count := Count  + Summary.Counters (K, S);
         end loop;
         if Summary.Counters (K, Any) /= Count then
            return False;
         end if;
      end loop;

      --  Check per-state summary and compute total count

      Total_Count := 0;

      for S in Task_State'Range loop
         Count := 0;
         for K in Task_Kind'Range loop
            Count := Count  + Summary.Counters (K, S);
            Total_Count := Total_Count  + Summary.Counters (K, S);
         end loop;
         if Summary.Counters (Any, S) /= Count then
            return False;
         end if;
      end loop;

      --  Check total count

      return Summary.Counters (Any, Any) = Total_Count;
   end Task_Summary_Valid;

end PolyORB.Task_Info;
