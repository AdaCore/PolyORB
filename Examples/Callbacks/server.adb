with Text_IO; use Text_IO;
with Types;   use Types;
with Common;  use Common;
with Scheduler; use Scheduler;
procedure Server is
   W : Any_Worker;
   Q : Query;
begin
   while Continue loop
      Pop (W, Q);
      if W /= null then
         Put_Line ("Query" & Q'Img & " is going to be executed");
         Work (W, Q, Save'Access);
      else
         delay 1.0;
      end if;
   end loop;
end Server;
