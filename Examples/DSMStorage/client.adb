with Vector;  use Vector;
with Scheduler;

procedure Client is
   Partition : constant Natural := Client'Partition_ID;
   Low, High : Natural;

begin

   -- Suspend partitions until they are all ready to go.

   Scheduler.Wait_For_All_Partitions (Partition);
   for Times in 0 .. N_Partitions - 1 loop
      Low  := (Block_Size * (Times + Partition - 1)) mod Length;
      High := (Block_Size * (Times + Partition) - 1) mod Length;
      for N in Low .. High loop
         Content (N) := Content (N) + Partition;
      end loop;
      --  Suspend partitions until they have all completed the
      --  computation on their current block.
      Scheduler.Wait_For_All_Partitions (Partition);
   end loop;
end Client;

