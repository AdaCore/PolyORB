with Ada.Text_IO; use Ada.Text_IO;
with Remote;      use Remote;

procedure Test_ACR is

begin
   if Call_OK (Test_ACR'Partition_ID) then
      Put_Line ("Call was on the same partition");
   else
      Put_Line ("Call was on different partition");
   end if;
end Test_ACR;
