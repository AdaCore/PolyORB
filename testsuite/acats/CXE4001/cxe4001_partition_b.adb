

-----------------------------------------------------------------------------

with CXE4001_Decl_Pure;
with CXE4001_Partition_A;
with Report;
package body CXE4001_Partition_B is

procedure Raise_Program_Error is
begin
  raise Program_Error;
end Raise_Program_Error;


procedure Raise_Visible_Exception is
begin
  raise CXE4001_Decl_Pure.Visible_User_Defined_Exception;
end Raise_Visible_Exception;


procedure Raise_Invisible_Exception is
  Invisible : exception;
begin
  raise Invisible;
end Raise_Invisible_Exception;


procedure Call_A_Raise_Invisible_1 is
begin
  CXE4001_Partition_A.Raise_Invisible;
  Report.Failed ("exception propagation in Call_A_Raise_Invisible_1");
end Call_A_Raise_Invisible_1;


-- Call_A_Raise_Invisible_2 differs from *_1 in that it handles the
-- invisible exception and then re-raises it.
procedure Call_A_Raise_Invisible_2 is
begin
  CXE4001_Partition_A.Raise_Invisible;
  Report.Failed ("exception propagation in Call_A_Raise_Invisible_2");
exception
  when others =>
    raise;  -- re-raise the invisible exception
end Call_A_Raise_Invisible_2;


-- the following task is used to keep this partition from
-- completing until partition A informs it that the test is 
-- finished.  This is done by calling the Finished procedure
-- in the specification of this package.

task Coordinate_Completion is
  entry Finished;
end Coordinate_Completion;

task body Coordinate_Completion is
begin
  accept Finished;
  Report.Result;
end Coordinate_Completion;

procedure Finished is
begin
  Coordinate_Completion.Finished;
end Finished;
end CXE4001_Partition_B;
