package Scheduler is
   pragma Remote_Call_Interface;

   procedure Wait_For_All_Partitions (P : Natural);
end Scheduler;
