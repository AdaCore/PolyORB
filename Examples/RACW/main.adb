with Ada.Text_IO; use Ada.Text_IO;
with Common; use Common;
with Controller; use Controller;
procedure Main is
   task type Anonymous_Task is
      pragma Storage_Size (150000);
      entry Start (D : Integer);
   end Anonymous_Task;
   task body Anonymous_Task is
      W : Worker_Access;
      J : Job;
   begin
      accept Start (D : Integer) do
	 J.Job_Duration := D;
      end Start;
      W := Get_Worker;
      Do_Job (W, J);
      Register (W);
   exception when others =>
      Put_Line ("Anonymous is dead");
   end Anonymous_Task;
   Table : array (1 .. 15) of Anonymous_Task;
begin
   for I in Table'Range loop
      Table (I).Start (I);
   end loop;
end Main;
