------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--             P O L Y O R B . T A S K I N G . R W _ L O C K S              --
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

--  Inter-process synchronisation objects.

--  $Id: //droopi/main/src/polyorb-tasking-rw_locks.adb#2 $

with Ada.Unchecked_Deallocation;

with PolyORB.Log;
with PolyORB.Tasking.Mutexes;

package body PolyORB.Tasking.Rw_Locks is

   use PolyORB.Log;
   use PolyORB.Tasking.Condition_Variables;
   use PolyORB.Tasking.Mutexes;

   package L is new PolyORB.Log.Facility_Log ("polyorb.locks");
   procedure O (Message : in String; Level : Log_Level := Debug)
     renames L.Output;

   Rw_Lock_Counter : Natural := 0;
   --  For debugging purposes.

   All_Rw_Locks : Tasking.Mutexes.Mutex_Access;

   procedure Create (L : out Rw_Lock_Access)
   is
      Result : constant Rw_Lock_Access
        := new Rw_Lock_Type;
   begin
      if All_Rw_Locks = null then
         Create (All_Rw_Locks);
      end if;
      pragma Assert (All_Rw_Locks /= null);
      Enter (All_Rw_Locks);
      Rw_Lock_Counter := Rw_Lock_Counter + 1;
      pragma Debug (O ("Rw_Lock: init/Serial ="
                       & Rw_Lock_Counter'Img));
      Result.Serial := Rw_Lock_Counter;
      Leave (All_Rw_Locks);
      Create (Result.Guard_Values);
      L := Result;
   end Create;

   procedure Free is new Ada.Unchecked_Deallocation
     (Rw_Lock_Type, Rw_Lock_Access);

   procedure Destroy (L : in out Rw_Lock_Access) is
   begin
      pragma Debug
        (O ("Rw_Lock: final/Serial =" & L.Serial'Img));
      Destroy (L.Guard_Values);
      Free (L);
   end Destroy;

   procedure Lock_W (L : access Rw_Lock_Type) is
   begin
      pragma Debug (O ("Lock_W Serial =" & L.Serial'Img));

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

   procedure Lock_R (L : access Rw_Lock_Type) is
   begin

      pragma Debug (O ("Lock_R"));

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

   procedure Unlock_W (L : access Rw_Lock_Type) is
   begin
      pragma Debug (O ("Unlock_W"));

      Enter (All_Rw_Locks);
      if L.Count /= -1 then
         raise Program_Error;
      else
         L.Count := 0;
      end if;
      Broadcast (L.Guard_Values);
      Leave (All_Rw_Locks);
   end Unlock_W;

   procedure Unlock_R (L : access Rw_Lock_Type) is
   begin
      pragma Debug (O ("Unlock_R"));

      Enter (All_Rw_Locks);
      if L.Count <= 0 then
         raise Program_Error;
      else
         L.Count := L.Count - 1;
      end if;
      Broadcast (L.Guard_Values);
      Leave (All_Rw_Locks);
   end Unlock_R;

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
