------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--        POLYORB.TASKING.PROFILES.FULL_TASKING.CONDITION_VARIABLES         --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2002-2006, Free Software Foundation, Inc.          --
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

--  Implementation of condition variables under the Full_Tasking profile.

with Ada.Unchecked_Deallocation;

with PolyORB.Initialization;

with PolyORB.Log;
with PolyORB.Utils.Strings;

package body PolyORB.Tasking.Profiles.Full_Tasking.Condition_Variables is

   use PolyORB.Log;

   package L is new PolyORB.Log.Facility_Log
     ("polyorb.tasking.profiles.full_tasking.condition_variables");

   procedure O (Message : String; Level : Log_Level := Debug)
     renames L.Output;
   function C (Level : Log_Level := Debug) return Boolean
     renames L.Enabled;
   pragma Unreferenced (C); --  For conditional pragma Debug

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
         pragma Debug (O ("Broadcast: will release:"
                          & Natural'Image (To_Free)
                          & " tasks."));
      end Broadcast;

      -------------------------
      -- Condition_PO.Signal --
      -------------------------

      entry Signal when To_Free = 0 is
      begin
         if Condition_PO.Wait'Count /= 0 then
            To_Free := 1;
         end if;
         pragma Debug (O ("Signal."));
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

   ---------------
   -- Broadcast --
   ---------------

   procedure Broadcast
     (Cond : access Full_Tasking_Condition_Type) is
   begin
      Cond.The_PO.Broadcast;
   end Broadcast;

   ------------
   -- Create --
   ------------

   function Create
     (MF   : access Full_Tasking_Condition_Factory_Type;
      Name : String := "") return PTCV.Condition_Access
   is
      pragma Warnings (Off);
      pragma Unreferenced (MF);
      pragma Unreferenced (Name);
      --  XXX The use of Name is not yet implemented
      pragma Warnings (On);

      Cond : constant Full_Tasking_Condition_Access
        := new Full_Tasking_Condition_Type;

   begin
      pragma Debug (O ("Create"));
      Cond.The_PO := new Condition_PO;
      return PTCV.Condition_Access (Cond);
   end Create;

   -------------
   -- Destroy --
   -------------

   procedure Free is new Ada.Unchecked_Deallocation
     (PTCV.Condition_Type'Class, PTCV.Condition_Access);

   procedure Free is new Ada.Unchecked_Deallocation
     (Condition_PO, Condition_PO_Access);

   procedure Destroy
     (MF   : access Full_Tasking_Condition_Factory_Type;
      Cond : in out PTCV.Condition_Access)
   is
      pragma Warnings (Off);
      pragma Unreferenced (MF);
      pragma Warnings (On);

   begin
      pragma Debug (O ("Destroy"));
      Free (Full_Tasking_Condition_Access (Cond).The_PO);
      Free (Cond);
   end Destroy;

   ------------
   -- Signal --
   ------------

   procedure Signal (Cond : access Full_Tasking_Condition_Type) is
   begin
      Cond.The_PO.Signal;
   end Signal;

   ----------
   -- Wait --
   ----------

   procedure Wait
     (Cond : access Full_Tasking_Condition_Type;
      M    : access PTM.Mutex_Type'Class)
   is
   begin
      pragma Debug (O ("Wait: enter"));
      Cond.The_PO.Release_Then_Wait (PTM.Mutex_Access (M));
      pragma Debug (O ("Wait: Leave"));
      PTM.Enter (M);
   end Wait;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize;

   procedure Initialize is
   begin
      PTCV.Register_Condition_Factory
        (PTCV.Condition_Factory_Access (The_Condition_Factory));
   end Initialize;

   use PolyORB.Initialization;
   use PolyORB.Initialization.String_Lists;
   use PolyORB.Utils.Strings;

begin
   Register_Module
     (Module_Info'
      (Name      => +"tasking.profiles.full_tasking.condition_variables",
       Conflicts => Empty,
       Depends   => Empty,
       Provides  => +"tasking.condition_variables",
       Implicit  => False,
       Init      => Initialize'Access,
       Shutdown  => null));
end PolyORB.Tasking.Profiles.Full_Tasking.Condition_Variables;
