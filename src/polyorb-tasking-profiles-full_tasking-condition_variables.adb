------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--             P O L Y O R B . T A S K I N G . P R O F I L E S              --
--    . F U L L _ T A S K I N G . C O N D I T I O N _ V A R I A B L E S     --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--             Copyright (C) 1999-2002 Free Software Fundation              --
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

--  Implementation of condition variables under the Full_Tasking profile.

with Ada.Unchecked_Deallocation;

with PolyORB.Initialization;
with PolyORB.Log;
with PolyORB.Utils.Strings;

package body PolyORB.Tasking.Profiles.Full_Tasking.Condition_Variables is

   use PolyORB.Log;

   package L is new PolyORB.Log.Facility_Log
     ("polyorb.tasking.profiles.full_tasking.condition_variables");

   procedure O (Message : in String; Level : Log_Level := Debug)
     renames L.Output;

   procedure Initialize;

   -----------------------------------------------------------------
   -- Underlying protected object for Full_Tasking_Condition_Type --
   -----------------------------------------------------------------

   protected type Condition_PO is

      entry Release_Then_Wait (M : PTM.Mutex_Access);
      --  Atomically release mutex M, then requeue on Wait.

      entry Wait;
      --  Real wait.

      entry Signal;
      --  Real implementation of Signal.

      entry Broadcast;
      --  Real implementation of Broadcast.

   private

      To_Free   : Natural := 0;
      --   Number of remaining tasks in the queue that must be freed.

   end Condition_PO;

   ----------
   -- Free --
   ----------

   procedure Free is new Ada.Unchecked_Deallocation
     (PTCV.Condition_Type'Class, PTCV.Condition_Access);

   procedure Free is new Ada.Unchecked_Deallocation
     (Condition_PO, Condition_PO_Access);

   ---------------
   -- Broadcast --
   ---------------

   procedure Broadcast
     (C : access Full_Tasking_Condition_Type) is
   begin
      C.The_PO.Broadcast;
   end Broadcast;

   ------------------
   -- Condition_PO --
   ------------------

   protected body Condition_PO is

      ----------------------------
      -- Condition_PO.Broadcast --
      ----------------------------

      entry Broadcast when To_Free = 0 is
      begin
         To_Free := Condition_PO.Wait'Count;
      end Broadcast;

      -------------------------
      -- Condition_PO.Signal --
      -------------------------

      entry Signal when To_Free = 0 is
      begin
         if Condition_PO.Wait'Count /= 0 then
            To_Free := 1;
         end if;
      end Signal;

      ------------------------------------
      -- Condition_PO.Release_Then_Wait --
      ------------------------------------

      entry Release_Then_Wait (M : PTM.Mutex_Access) when True is
      begin
         PTM.Leave (M);
         requeue Condition_PO.Wait;
      end Release_Then_Wait;

      -----------------------
      -- Condition_PO.Wait --
      -----------------------

      entry Wait when To_Free > 0 is
      begin
         To_Free := To_Free - 1;
      end Wait;

   end Condition_PO;

   ------------
   -- Create --
   ------------

   function Create
     (MF   : access Full_Tasking_Condition_Factory_Type;
      Name : String := "")
      return PTCV.Condition_Access
   is
      pragma Warnings (Off);
      pragma Unreferenced (MF);
      pragma Unreferenced (Name);
      --  XXX The use of Name is not yet implemented
      pragma Warnings (On);

      C : Full_Tasking_Condition_Access := new Full_Tasking_Condition_Type;

   begin
      pragma Debug (O ("create condition variable"));
      C.The_PO := new Condition_PO;
      return PTCV.Condition_Access (C);
   end Create;

   -------------
   -- Destroy --
   -------------

   procedure Destroy
     (MF : access Full_Tasking_Condition_Factory_Type;
      C  : in out PTCV.Condition_Access)
   is
      pragma Warnings (Off);
      pragma Unreferenced (MF);
      pragma Warnings (On);

   begin
      pragma Debug (O ("destroy condition variable"));
      Free (Full_Tasking_Condition_Access (C).The_PO);
      Free (C);
   end Destroy;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is
   begin
      pragma Debug
        (O ("initialize package"
            & " tasking.profiles.full_tasking.condition_variables"));
      PTCV.Register_Condition_Factory
        (PTCV.Condition_Factory_Access (The_Condition_Factory));
   end Initialize;

   ------------
   -- Signal --
   ------------

   procedure Signal
     (C : access Full_Tasking_Condition_Type)
   is
   begin
      pragma Debug (O ("stabilisation done, signal condition variable"));
      C.The_PO.Signal;
   end Signal;

   ----------
   -- Wait --
   ----------

   procedure Wait
     (C : access Full_Tasking_Condition_Type;
      M : access PTM.Mutex_Type'Class)
   is
   begin
      C.The_PO.Release_Then_Wait (PTM.Mutex_Access (M));
      pragma Debug (O ("wait ended"));
      PTM.Enter (M);
   end Wait;

   use PolyORB.Initialization;
   use PolyORB.Initialization.String_Lists;
   use PolyORB.Utils.Strings;

begin
   Register_Module
     (Module_Info'
      (Name => +"tasking.profiles.full_tasking.condition_variables",
       Conflicts => Empty,
       Depends => Empty,
       Provides => +"tasking.condition_variables",
       Init => Initialize'Access));
end PolyORB.Tasking.Profiles.Full_Tasking.Condition_Variables;
