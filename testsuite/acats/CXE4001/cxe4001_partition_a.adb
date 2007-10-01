pragma Style_Checks (Off);

-----------------------------------------------------------------------------

with CXE4001_Decl_Pure;
with CXE4001_Partition_B;
with Report;
with System.RPC;

package body CXE4001_Partition_A is

  -- the following exception is only defined in partition A
  Exception_Local_To_A : exception;

procedure Predefined_Simple is
begin
  if CXE4001_Decl_Pure.Verbose then
     Report.Comment ("Check that a predefined exception can be raised" &
                     " in one partition and handled in another");
  end if;

  CXE4001_Partition_B.Raise_Program_Error;
  Report.Failed ("Predefined exception was not propagated across partitions");
exception
  when Program_Error =>
     null;  -- passed
  when System.RPC.Communication_Error =>
     Report.Failed ("Communication_Error occurred");
  when others =>
     Report.Failed ("Wrong exception was propagated across partitions (1)");
end Predefined_Simple;

procedure Userdefined_Simple is
begin
  if CXE4001_Decl_Pure.Verbose then
     Report.Comment ("Check that an exception declared in a shared " &
                     " package can be raised in one partition and" &
                     " handled in another");
  end if;
  CXE4001_Partition_B.Raise_Visible_Exception;
  Report.Failed ("Shared exception was not propagated across partitions");
exception
  when CXE4001_Decl_Pure.Visible_User_Defined_Exception =>
     null;  -- passed
  when System.RPC.Communication_Error =>
     Report.Failed ("Communication_Error occurred");
  when others =>
     Report.Failed ("Wrong exception was propagated across partitions (2)");
end Userdefined_Simple;

procedure Invisible_Simple is
begin
  if CXE4001_Decl_Pure.Verbose then
     Report.Comment ("Check that an exception declared in another" &
                     " partition can be handled with an 'others'" &
                     " exception handler");
  end if;
  CXE4001_Partition_B.Raise_Invisible_Exception;
  Report.Failed ("Invisible exception was not propagated across partitions");
exception
  when System.RPC.Communication_Error =>
     Report.Failed ("Communication_Error occurred");
  when others =>
     null;  -- passed
end Invisible_Simple;

procedure Invisible_Complex_1 is
begin
  if CXE4001_Decl_Pure.Verbose then
     Report.Comment ("Check that an exception declared in this partition" &
                     " and not visible to the other partition is properly" &
                     " handled when it propagates from this partition," &
                     " through the other partition, and back to this" &
                     " partition.");
  end if;
  CXE4001_Partition_B.Call_A_Raise_Invisible_1;
  Report.Failed ("Local exception was not propagated across partitions");
exception
  when Exception_Local_To_A =>
     null;  -- passed
  when System.RPC.Communication_Error =>
     Report.Failed ("Communication_Error occurred");
  when others =>
     Report.Failed ("Wrong exception was propagated across partitions (3)");
end Invisible_Complex_1;

procedure Invisible_Complex_2 is
begin
  if CXE4001_Decl_Pure.Verbose then
     Report.Comment ("Check that an exception declared in this partition" &
                     " and not visible to the other partition is properly" &
                     " handled when it propagates from this partition," &
                     " is handled and then re-raised in the other" &
                     " partition, and propagated back to this" &
                     " partition.");
  end if;
  CXE4001_Partition_B.Call_A_Raise_Invisible_2;
  Report.Failed ("Local re-raised exception was not propagated" &
                 " across partitions");
exception
  when Exception_Local_To_A =>
     null;  -- passed
  when System.RPC.Communication_Error =>
     Report.Failed ("Communication_Error occurred");
  when others =>
     Report.Failed ("Wrong exception was propagated across partitions (4)");
end Invisible_Complex_2;

    -- service routine for Invisible_Complex test
procedure Raise_Invisible is
begin
  -- Report.Comment ("Round-trip call complete. ");
  raise Exception_Local_To_A;
end Raise_Invisible;

end CXE4001_Partition_A;
