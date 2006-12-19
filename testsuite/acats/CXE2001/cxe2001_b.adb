pragma Style_Checks (Off);
pragma Warnings (Off);

-----------------------------------------------------------------------------
--
-- main procedure for partition B
--

with CXE2001_Part_B;
with CXE2001_Shared;
with Report;
procedure CXE2001_B is
begin
  -- The body of package CXE2001_Part_B contains a task that is elaborated
  -- and will prevent the partition from completing until a Test_Finished
  -- call arrives from partition A.
  Report.Test ("CXE2001_B", "Access to shared passive data from active" &
                            " partitions (server)");
end CXE2001_B;
