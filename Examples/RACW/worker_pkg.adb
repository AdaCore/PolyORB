with Common;
with Controller;
package body Worker_Pkg is
   procedure Do_Job (W : access Real_Worker; J : Job) is
      D : Integer := J.Job_Duration;
      S : Integer := 0;
   begin
      while D > 0 loop
         delay 0.3;
         D := D - W.Speed;
         S := S + 1;
      end loop;
      Controller.Done ("Job (" & Integer'Image (S) &
                       " timeslots) at speed" & Integer'Image (W.Speed));
   end Do_Job;
   Local : aliased Real_Worker;
begin
   Controller.Get_Integer ("Speed : ", Local.Speed);
   Controller.Register (Local'Access);
end Worker_Pkg;
