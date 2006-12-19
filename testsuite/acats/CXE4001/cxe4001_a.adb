pragma Style_Checks (Off);
pragma Warnings (Off);

-----------------------------------------------------------------------------

with CXE4001_Partition_A;
with CXE4001_Partition_B;
with Report;
with System.RPC;
procedure CXE4001_A is
begin
  Report.Test ("CXE4001_A", "Exception handling across partitions");
  delay 2.0; --  XXX @@ allow other part'n to start (actually should
             --  wait for it to become ready!

  -- make sure partitioning is performed
  if CXE4001_Partition_A'Partition_ID = CXE4001_Partition_B'Partition_ID then
    Report.Failed ("Partitioning Error - CXE4001_Partition_A and " &
                   "CXE4001_Partition_B are in the same partition.");
  end if;

  -- now do the tests
  CXE4001_Partition_A.Predefined_Simple;
  CXE4001_Partition_A.Userdefined_Simple;
  CXE4001_Partition_A.Invisible_Simple;
  CXE4001_Partition_A.Invisible_Complex_1;
  CXE4001_Partition_A.Invisible_Complex_2;

  -- all done
  CXE4001_Partition_B.Finished;
  Report.Result;
end CXE4001_A;
