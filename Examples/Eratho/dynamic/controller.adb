with Common; use Common;
package body Controller is

   Size  : constant := 3;
   Table : array (1 .. Size) of Prime_Pool_Access;
   Last  : Natural := 0;

   protected Keeper is
      entry Ready;
      procedure Start;
   private
      Full : Boolean := False;
   end Keeper;

   protected body Keeper is
      entry Ready when Full is
      begin null; end Ready;
      procedure Start is
      begin Full := True; end Start;
   end Keeper;
      
   procedure Register
     (Pool  : in  Prime_Pool_Access;
      Index : out Natural) is
   begin
      Last := Last + 1;
      Index := Last;
      Table (Last) := Pool;
      if Last = Size then
         Keeper.Start;
      end if;
   end Register;

   function  Next
     (Index : in Natural)
      return Prime_Pool_Access is
   begin
      Keeper.Ready;
      if Index = Size then
         return Table (1);
      else
         return Table (Index + 1);
      end if;
   end Next;

   function First
      return Prime_Pool_Access is
   begin
      Keeper.Ready;
      return Table (1);
   end First;

end Controller;
