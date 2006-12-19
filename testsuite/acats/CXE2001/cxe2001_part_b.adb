pragma Style_Checks (Off);
-----------------------------------------------------------------------------

with Report;
with CXE2001_Shared;
package body CXE2001_Part_B is
  task Keep_Partition_Alive is
    entry Quit;
  end Keep_Partition_Alive;

  task body Keep_Partition_Alive is
  begin
    accept Quit;
  end Keep_Partition_Alive;

  procedure Test_Finished is
  begin
    Keep_Partition_Alive.Quit;
    Report.Result;
  end Test_Finished;

  procedure Set_Shared_Data (Value : Integer) is
  begin
    CXE2001_Shared.Shared_Data := Value;
  end Set_Shared_Data;

  procedure Increment_Counter is
  begin
    CXE2001_Shared.Shared_Counter.Increment;
  end Increment_Counter;
end CXE2001_Part_B;
