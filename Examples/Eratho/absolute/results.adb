--  User Defined Libraries
with Common; use Common;

package body Results is

   protected Keeper is
      entry Load
        (Divider : out Natural;
         Where   : out Partition_ID);
      procedure Save
        (Divider : in Natural;
         Where   : in Partition_ID);
   private
      Arrived       : Boolean := False;
      Saved_Divider : Natural;
      Saved_Where   : Partition_ID;
   end Keeper;

   protected body Keeper is
      entry Load
        (Divider : out Natural;
         Where   : out Partition_ID) when Arrived is
      begin
         Divider := Saved_Divider;
         Where   := Saved_Where;
         Arrived := False;
      end Load;
      procedure Save
        (Divider : in Natural;
         Where   : in Partition_ID) is
      begin
         Saved_Divider := Divider;
         Saved_Where   := Where;
         Arrived       := True;
      end Save;
   end Keeper;

   procedure Save
     (Divider : in Natural;
      Where   : in Partition_ID) is
   begin
      Keeper.Save (Divider, Where);
   end Save;

   procedure Load_When_Ready
     (Divider : out Natural;
      Where   : out Partition_ID) is
   begin
      Keeper.Load (Divider, Where);
   end Load_When_Ready;

end Results;

