--  Inter-process synchronisation objects.

--  $Id: //droopi/main/src/droopi-locks.adb#1 $

with Ada.Unchecked_Deallocation;

with Droopi.Log;
with Droopi.Soft_Links; use Droopi.Soft_Links;

package body Droopi.Locks is

   use Droopi.Log;

   package L is new Droopi.Log.Facility_Log ("droopi.locks");
   procedure O (Message : in String; Level : Log_Level := Debug)
     renames L.Output;

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

   procedure Lock_W (L : access Rw_Lock_Type)
   is
      Version : Version_Id;
   begin
      pragma Debug (O ("Lock_W Serial =" & L.Serial'Img));

      loop
         Enter_Critical_Section;
         L.Writers_Waiting := L.Writers_Waiting + 1;
         exit when L.Count = 0;
         Lookup (L.Guard_Values.all, Version);
         Leave_Critical_Section;

         Differ (L.Guard_Values.all, Version);
         --  Wait until the condition may have changed
         --  from the value it had when we were within
         --  the critical section.
      end loop;

      L.Count := -1;
      L.Writers_Waiting := L.Writers_Waiting - 1;
      Leave_Critical_Section;
   end Lock_W;

   procedure Lock_R (L : access Rw_Lock_Type)
   is
      Version : Version_Id;
   begin

      pragma Debug (O ("Lock_R"));

      loop
         Enter_Critical_Section;
         L.Readers_Waiting := L.Readers_Waiting + 1;

         exit when L.Count >= 0
           and then L.Count < L.Max_Count
           and then L.Writers_Waiting = 0;

         Lookup (L.Guard_Values.all, Version);
         Leave_Critical_Section;

         Differ (L.Guard_Values.all, Version);
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
      Update (L.Guard_Values.all);
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
      Update (L.Guard_Values.all);
      Leave_Critical_Section;
   end Unlock_R;

   procedure Set_Max_Count
     (L : access Rw_Lock_Type;
      Max : Natural) is
   begin
      Enter_Critical_Section;
      L.Max_Count := Max;
      Update (L.Guard_Values.all);
      Leave_Critical_Section;
   end Set_Max_Count;

end Droopi.Locks;
