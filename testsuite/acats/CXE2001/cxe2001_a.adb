pragma Style_Checks (Off);
-----------------------------------------------------------------------------
--
-- main procedure for partition A (and the test)
--

with CXE2001_Part_B;
with CXE2001_Shared;
with System.RPC;
with Report;
procedure CXE2001_A is
  use type System.RPC.Partition_ID;
begin
  Report.Test ("CXE2001_A", "Access to shared passive data from active" &
                          " partitions");

  -- make sure partitioning is performed correctly
  if CXE2001_A'Partition_ID = CXE2001_Part_B'Partition_ID then
    Report.Failed ("Partitioning Error - CXE2001_A and CXE2001_Part_B" &
                   " are in the same partition.");
  end if;

  -- It doesn't really matter which partition the shared data is placed in
  -- so we don't check that it is in a particular partition.

  -- check the shared data access
  CXE2001_Shared.Shared_Data := Report.Ident_Int(42);
  if CXE2001_Shared.Shared_Data /= 42 then
    Report.Failed ("direct assignment to shared data failed");
  end if;

  CXE2001_Part_B.Set_Shared_Data (Report.Ident_Int(45));
  case CXE2001_Shared.Shared_Data is
    when 42 => Report.Failed ("remote access to the shared data failed");
    when 45 => null;  -- expected result
               -- Report.Comment ("remote access to shared passive data");
    when others => Report.Failed ("unexpected value in shared data (1)" &
                             Integer'Image (CXE2001_Shared.Shared_Data));
  end case;

  -- check the protected object access  
  declare
     V : Integer := CXE2001_Shared.Shared_Counter.Value;
  begin
     if V /= 0 then
       Report.Failed ("initial value of shared protected value is" &
                      Integer'Image (V));
     end if;

     -- manipulate the protected object directly
     CXE2001_Shared.Shared_Counter.Increment;
     if CXE2001_Shared.Shared_Counter.Value /= 1 then
       Report.Failed ("incorrect shared passive protected object value 1");
     end if;

     CXE2001_Shared.Shared_Counter.Increment;
     if CXE2001_Shared.Shared_Counter.Value /= 2 then
       Report.Failed ("incorrect shared passive protected object value 2");
     end if;

     CXE2001_Shared.Shared_Counter.Increment;
     if CXE2001_Shared.Shared_Counter.Value /= 3 then
       Report.Failed ("incorrect shared passive protected object value 3");
     end if;

     -- manipulate the protected object from the other partition
     CXE2001_Part_B.Increment_Counter;
     V := CXE2001_Shared.Shared_Counter.Value;
     if V = 3 then
       Report.Failed ("shared passive protected object appears " &
                      "to be copied");
     elsif V = 4 then
       null;
       -- Report.Comment ("remote access to shared passive protected" &
       --                 " object");
     else
       Report.Failed ("unexpected value in shared data (2)" &
           Integer'Image (V));
     end if;
     
  end;

  -- finish up
  CXE2001_Part_B.Test_Finished;
  Report.Result;
end CXE2001_A;
