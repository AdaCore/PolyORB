--  User Defined Libraries
with Common; use Common;

package body Controller is

   Global_Pool_Table  : array (Pool_Index) of Prime_Pool_Access;
   Current_Pool_Index : Pool_Index := 0;

   protected Barrier is
      entry Wait;
      procedure Signal;
   private
      Full : Boolean := False;
   end Barrier;

   protected body Barrier is
      entry Wait when Full is
      begin
         null;
      end Wait;
      procedure Signal is
      begin
         Full := True;
      end Signal;
   end Barrier;

   procedure Register
     (Pool  : in  Prime_Pool_Access;
      Index : out Pool_Index) is
   begin
      Current_Pool_Index := Current_Pool_Index + 1;
      Index := Current_Pool_Index;
      Global_Pool_Table (Current_Pool_Index) := Pool;
      if Current_Pool_Index = Last_Pool_Index then
         Barrier.Signal;
      end if;
   end Register;

   function  Next
     (Index : in Pool_Index)
      return Prime_Pool_Access is
   begin
      Barrier.Wait;
      if Index = Last_Pool_Index then
         return Global_Pool_Table (1);
      else
         return Global_Pool_Table (Index + 1);
      end if;
   end Next;

   function First
      return Prime_Pool_Access is
   begin
      Barrier.Wait;
      return Global_Pool_Table (1);
   end First;

end Controller;
