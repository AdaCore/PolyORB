

with CXE4001_Partition_B;
with CXE4001_Partition_A;
with Report;
with System.RPC; 
procedure CXE4001_B is
begin
  Report.Test ("CXE4001_B", "Server partition of exception handling test");
  if CXE4001_Partition_A'Partition_ID = CXE4001_Partition_B'Partition_ID then
    Report.Failed ("Partitioning Error - 1 and Part_B are in the" &
                   " same partition.");
  end if;
  -- Report.Result is called in the body of CXE4001_Partition_B.
end CXE4001_B;
