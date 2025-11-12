------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--             P O L Y O R B . T A S K I N G . R W _ L O C K S              --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2002-2012, Free Software Foundation, Inc.          --
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

--  Inter-process synchronisation objects.

with PolyORB.Utils.Unchecked_Deallocation;

with PolyORB.Log;
with PolyORB.Tasking.Mutexes;

package body PolyORB.Tasking.Rw_Locks is

   use PolyORB.Log;
   use PolyORB.Tasking.Condition_Variables;
   use PolyORB.Tasking.Mutexes;

   package L is new PolyORB.Log.Facility_Log ("polyorb.tasking.rw_locks");
   procedure O (Message : String; Level : Log_Level := Debug)
     renames L.Output;
   function C (Level : Log_Level := Debug) return Boolean
     renames L.Enabled;

   Rw_Lock_Counter : Natural := 0;
   --  For debugging purposes.

   All_Rw_Locks : Tasking.Mutexes.Mutex_Access;

   ------------
   -- Create --
   ------------

   procedure Create (L : out Rw_Lock_Access)
   is
      Result : constant Rw_Lock_Access := new Rw_Lock_Type;

   begin
      if All_Rw_Locks = null then
         Create (All_Rw_Locks);
      end if;
      pragma Assert (All_Rw_Locks /= null);

      Enter (All_Rw_Locks);

      Rw_Lock_Counter := Rw_Lock_Counter + 1;
      Result.Serial := Rw_Lock_Counter;
      pragma Debug (C, O ("Create, Serial ="
                       & Integer'Image (Result.Serial)));

      Leave (All_Rw_Locks);
      Create (Result.Guard_Values);
      L := Result;
   end Create;

   ----------
   -- Free --
   ----------

   procedure Free is new PolyORB.Utils.Unchecked_Deallocation.Free
     (Object => Rw_Lock_Type,
      Name => Rw_Lock_Access);

   -------------
   -- Destroy --
   -------------

   procedure Destroy (L : in out Rw_Lock_Access) is
   begin
      pragma Debug (C, O ("Destroy, Serial =" & Integer'Image (L.Serial)));

      Destroy (L.Guard_Values);
      Free (L);
      pragma Debug (C, O ("Desroy: end"));
   end Destroy;

   --------------
   -- Is_Set_R --
   --------------

   function Is_Set_R (L : access Rw_Lock_Type)
                     return Boolean is
   begin
      return L.Count > 0;
   end Is_Set_R;

   --------------
   -- Is_Set_W --
   --------------

   function Is_Set_W (L : access Rw_Lock_Type)
                     return Boolean is
   begin
      return L.Count = -1;
   end Is_Set_W;

   ------------
   -- Lock_W --
   ------------

   procedure Lock_W (L : access Rw_Lock_Type) is
   begin
      pragma Debug (C, O ("Lock_W Serial =" & Integer'Image (L.Serial)));

      Enter (All_Rw_Locks);

      while L.Count /= 0 loop
         L.Writers_Waiting := L.Writers_Waiting + 1;
         Wait (L.Guard_Values, All_Rw_Locks);
         L.Writers_Waiting := L.Writers_Waiting - 1;
         --  Wait until the condition may have changed
         --  from the value it had when we were within
         --  the critical section.
      end loop;

      L.Count := -1;
      Leave (All_Rw_Locks);
   end Lock_W;

   ------------
   -- Lock_R --
   ------------

   procedure Lock_R (L : access Rw_Lock_Type) is
   begin
      pragma Debug (C, O ("Lock_R Serial =" & Integer'Image (L.Serial)));

      Enter (All_Rw_Locks);

      while not (True
        and then L.Count >= 0
        and then L.Count < L.Max_Count
        and then L.Writers_Waiting = 0)
      loop
         L.Readers_Waiting := L.Readers_Waiting + 1;
         Wait (L.Guard_Values, All_Rw_Locks);
         L.Readers_Waiting := L.Readers_Waiting - 1;
      end loop;

      L.Count := L.Count + 1;
      Leave (All_Rw_Locks);
   end Lock_R;

   --------------
   -- Unlock_W --
   --------------

   procedure Unlock_W (L : access Rw_Lock_Type) is
   begin
      pragma Debug (C, O ("Unlock_W Serial =" & Integer'Image (L.Serial)));

      Enter (All_Rw_Locks);

      if L.Count /= -1 then
         pragma Debug (C, O ("Lock has not been previously taken !"));
         raise Program_Error;
      else
         L.Count := 0;
      end if;

      Broadcast (L.Guard_Values);
      Leave (All_Rw_Locks);
   end Unlock_W;

   --------------
   -- Unlock_R --
   --------------

   procedure Unlock_R (L : access Rw_Lock_Type) is
   begin
      pragma Debug (C, O ("Unlock_R Serial =" & Integer'Image (L.Serial)));

      Enter (All_Rw_Locks);

      if L.Count <= 0 then
         raise Program_Error;
      else
         L.Count := L.Count - 1;
      end if;

      Broadcast (L.Guard_Values);
      Leave (All_Rw_Locks);
   end Unlock_R;

   -------------------
   -- Set_Max_Count --
   -------------------

   procedure Set_Max_Count
     (L : access Rw_Lock_Type;
      Max : Natural) is
   begin
      Enter (All_Rw_Locks);

      L.Max_Count := Max;
      Broadcast (L.Guard_Values);

      Leave (All_Rw_Locks);
   end Set_Max_Count;

end PolyORB.Tasking.Rw_Locks;
