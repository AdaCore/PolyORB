with RCI_3;
with System.Garlic.Partitions;
package body RCI_2 is
   Done : Boolean := False;

   procedure Dump_Partition_Table is
   begin
      if not Done then
         Done := True;
         RCI_3.Dump_Partition_Table;
         delay 0.2;
         System.Garlic.Partitions.Dump_Partition_Table;
      end if;
   end Dump_Partition_Table;
end RCI_2;
