------------------------------------------------------------------------------
--                                                                          --
--                          ADABROKER COMPONENTS                            --
--                                                                          --
--                          B R O C A . L O C K S                           --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1999-2001 ENST Paris University, France.          --
--                                                                          --
-- AdaBroker is free software; you  can  redistribute  it and/or modify it  --
-- under terms of the  GNU General Public License as published by the  Free --
-- Software Foundation;  either version 2,  or (at your option)  any  later --
-- version. AdaBroker  is distributed  in the hope that it will be  useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License  for more details.  You should have received  a copy of the GNU  --
-- General Public License distributed with AdaBroker; see file COPYING. If  --
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
--             AdaBroker is maintained by ENST Paris University.            --
--                     (email: broker@inf.enst.fr)                          --
--                                                                          --
------------------------------------------------------------------------------

with Ada.Unchecked_Deallocation;

with Broca.Debug;
with Broca.Soft_Links; use Broca.Soft_Links;

package body Broca.Locks is

   -----------
   -- Debug --
   -----------

   Flag : constant Natural
     := Broca.Debug.Is_Active ("broca.locks");
   procedure O is new Broca.Debug.Output (Flag);

   protected body Mutex_Type is
      entry Lock when Owner = Null_Task_Id is
      begin
         Owner := Lock'Caller;
      end Lock;

      procedure Unlock is
      begin
         if Owner /= Current_Task then
            raise Program_Error;
         else
            Owner := Null_Task_Id;
         end if;
      end Unlock;

      procedure Check_Owner is
      begin
         if Owner /= Current_Task then
            raise Program_Error;
         end if;
      end Check_Owner;

      procedure Trylock (Success : out Boolean) is
      begin
         if Owner /= Null_Task_Id then
            Success := False;
            return;
         else
            Owner := Current_Task;
            Success := True;
            return;
         end if;
      end Trylock;
   end Mutex_Type;

   protected body Bcast_Lock_Type is
      entry Wait
      when Locked = False is
      begin
         null;
      end Wait;

      procedure Lock is
      begin
         if Locked then
            raise Program_Error;
         else
            Locked := True;
         end if;
      end Lock;

      procedure Unlock is
      begin
         if Locked then
            Locked := False;
         else
            Locked := True;
         end if;
      end Unlock;
   end Bcast_Lock_Type;

   Rw_Lock_Counter : Natural := 0;

   procedure Create (L : out Rw_Lock_Access)
   is
      Result : constant Rw_Lock_Access
        := new Rw_Lock_Type;
   begin
      Enter_Critical_Section;
      Rw_Lock_Counter := Rw_Lock_Counter + 1;
      pragma Debug (O ("Rw_Lock: init/Serial ="
                       & Rw_Lock_Counter'Img));
      Result.Serial := Rw_Lock_Counter;
      Leave_Critical_Section;
      Create (Result.Readers_Barrier);
      Create (Result.Writers_Barrier);
      L := Result;
   end Create;

   procedure Free is new Ada.Unchecked_Deallocation
     (Rw_Lock_Type, Rw_Lock_Access);

   procedure Destroy (L : in out Rw_Lock_Access) is
   begin
      pragma Debug
        (O ("Rw_Lock: final/Serial =" & L.Serial'Img));
      Destroy (L.Readers_Barrier);
      Destroy (L.Writers_Barrier);
      Free (L);
   end Destroy;

   procedure Lock_W (L : access Rw_Lock_Type) is
   begin
      pragma Debug (O ("Lock_W Serial =" & L.Serial'Img));

      loop
         Enter_Critical_Section;
         L.Writers_Waiting := L.Writers_Waiting + 1;
         exit when L.Count = 0;
         Leave_Critical_Section;

         --  XXX RACE CONDITION!
         --  Signal could be called here -- and lost...
         --  ... before we enter Wait.
         --  (probably same elsewhere in this unit.)

         Wait (L.Writers_Barrier.all);
      end loop;

      L.Count := -1;
      L.Writers_Waiting := L.Writers_Waiting - 1;
      Leave_Critical_Section;
   end Lock_W;

   procedure Lock_R (L : access Rw_Lock_Type) is
   begin

      pragma Debug (O ("Lock_R"));

      loop
         Enter_Critical_Section;
         L.Readers_Waiting := L.Readers_Waiting + 1;

         exit when L.Count >= 0
           and then L.Count < L.Max_Count
           and then L.Writers_Waiting = 0;

         Leave_Critical_Section;
         Wait (L.Readers_Barrier.all);
      end loop;

      L.Count := L.Count + 1;
      L.Readers_Waiting := L.Readers_Waiting - 1;
      Leave_Critical_Section;
   end Lock_R;

   procedure Unlock_W (L : access Rw_Lock_Type) is
   begin
      pragma Debug (O ("Unlock_W"));

      Enter_Critical_Section;
      if L.Count /= -1 then
         raise Program_Error;
      else
         L.Count := 0;
      end if;
      Signal_All (L.Readers_Barrier.all, False);
      Signal (L.Writers_Barrier.all);
      Leave_Critical_Section;
   end Unlock_W;

   procedure Unlock_R (L : access Rw_Lock_Type) is
   begin
      pragma Debug (O ("Unlock_R"));

      Enter_Critical_Section;
      if L.Count <= 0 then
         raise Program_Error;
      else
         L.Count := L.Count - 1;
      end if;
      Signal_All (L.Readers_Barrier.all, False);
      Signal (L.Writers_Barrier.all);
      Leave_Critical_Section;
   end Unlock_R;

   procedure Set_Max_Count
     (L : access Rw_Lock_Type;
      Max : Natural) is
   begin
      Enter_Critical_Section;
      L.Max_Count := Max;
      Leave_Critical_Section;
   end Set_Max_Count;

end Broca.Locks;
