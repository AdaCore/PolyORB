--  XXX
with Ada.Task_Identification; use Ada.Task_Identification;
with Ada.Text_IO; use Ada.Text_IO;
--  XXX

package body Broca.Locks is

   protected body Mutex_Type is
      entry Lock when Owner = Null_Task_Id is
      begin
         Owner := Lock'Caller;
         Put_Line ("In Lock, Owner = " & Image (Owner));
      end Lock;

      procedure Unlock is
      begin
         --  XXX
         Put_Line ("In Unlock, Owner = " & Image (Owner)
                   & ", Current = " & Image (Current_Task));
         --  XXX
         if Owner /= Current_Task then
            Put_Line ("Unlock is committing suicide as a"
                      & " consequence of schizophrenia.");
            raise Program_Error;
         else
            Owner := Null_Task_Id;
            Put_Line ("Did unlock, returning to caller.");
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

   protected body Rw_Lock_Type is
      entry Lock_W when Count = 0 is
      begin
         R_Prevented := False;
         Count := -1;
      end Lock_W;

      entry Lock_R
      when Count >= 0
        and Count < Max_Count
        and Lock_W'Count = 0
        and not R_Prevented is
      begin
         Count := Count + 1;
      end Lock_R;

      procedure Unlock_W is
      begin
         if Count /= -1 then
            raise Program_Error;
         else
            Count := 0;
         end if;
      end Unlock_W;

      procedure Unlock_R is
      begin
         if Count <= 0 then
            raise Program_Error;
         else
            Count := Count - 1;
         end if;
      end Unlock_R;

      procedure Prevent_R is
      begin
         R_Prevented := True;
      end Prevent_R;

      procedure Set_Max_Count (Max : Natural) is
      begin
         Max_Count := Max;
      end Set_Max_Count;
   end Rw_Lock_Type;

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
end Broca.Locks;
