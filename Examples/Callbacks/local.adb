with Scheduler; use Scheduler;
package body Local is

   Me : aliased New_Worker;
   ID : constant Natural := Local'Partition_ID;

   procedure Initialize is
   begin
      Push (Me'Access);
   end Initialize;

   procedure Work
     (W : access New_Worker;
      Q : in Query;
      C : in Callback) is
      R : Reply;
   begin
      delay Duration (Q);
      R := Reply (Q * Q);
      C.all (Q, R, ID);
      Push (Me'Access);
   end Work;

end Local;
