with Vector; use Vector;

package body Scheduler is

   protected Engine is
      entry Suspend;
      entry Resume;
   private
      Resuming : Boolean := False;
   end Engine;

   protected body Engine is
      entry Suspend when not Resuming is
      begin
         Resuming := (Resume'Count = N_Partitions - 1);
         requeue Resume;
      end Suspend;
      entry Resume when Resuming is
      begin
         Resuming := (Resume'Count /= 0);
      end Resume;
   end Engine;

   procedure Wait_For_All_Partitions (P : Natural) is
   begin
      delay 2.0;
      Engine.Suspend;
   end Wait_For_All_Partitions;

end Scheduler;
