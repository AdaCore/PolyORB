

package CXE4001_Partition_A is
  pragma Remote_Call_Interface;

    -- these procedures are the control actions for each test
  procedure Predefined_Simple;
  procedure Userdefined_Simple;
  procedure Invisible_Simple;
  procedure Invisible_Complex_1;
  procedure Invisible_Complex_2;

    -- service routine for Invisible_Complex test
  procedure Raise_Invisible;
end CXE4001_Partition_A;
