
-----------------------------------------------------------------------------

with CXE4006_Common;  use CXE4006_Common;
with CXE4006_Normal;
with CXE4006_Part_A1;
with CXE4006_Part_A2;
with CXE4006_Part_B;
with Report;
procedure CXE4006_B is

-----------------------
--
--      Service Routine

procedure Start_Test (Test_Number : Integer) is
begin
  if Verbose then
    Report.Comment ("starting test" & Integer'Image (Test_Number));
  end if;
end Start_Test;

-----------------------
--
--      Check that calls can be made to remote procedures when a
--      dispatching call is made where the controlling operand
--      designates a type declared in a remote call interface package.

procedure Dispatching_Test is

  Root_Obj   : CXE4006_Common.Root_Tagged_Type;
  A1_1_Obj   : CXE4006_Part_A1.A1_Tagged_Type_1;
  A1_2_Obj   : CXE4006_Part_A1.A1_Tagged_Type_2;
  A2_Obj     : CXE4006_Part_A2.A2_Tagged_Type;
  B_Obj      : CXE4006_Part_B.B_Tagged_Type;

  Callee     : Type_Decl_Location;
begin
  if Verbose then
    Report.Comment ("starting dispatching test");
  end if;

  Start_Test (1001);  -- not remote
  Root_Obj.Common_Record_Field := 100;
  Single_Controlling_Operand (
       Root_Tagged_Type'Class(Root_Obj),
       1001, Callee);
  if Root_Obj.Common_Record_Field /= 105 then
    Report.Failed ("test 1001 expected 105 received" &
                   Integer'Image (Root_Obj.Common_Record_Field));
  end if;
  if Callee /= Common_Spec then
    Report.Failed ("test 1001 callee was " &
                   Type_Decl_Location'Image (Callee));
  end if;


  Start_Test (1002);  -- remote
  A1_1_Obj.Common_Record_Field := 110;
  Single_Controlling_Operand (
       Root_Tagged_Type'Class(A1_1_Obj),
       1002, Callee);
  if A1_1_Obj.Common_Record_Field /= 116 then
    Report.Failed ("test 1002 expected 116 received" &
                   Integer'Image (A1_1_Obj.Common_Record_Field));
  end if;
  if Callee /= Part_A1_1_Spec then
    Report.Failed ("test 1002 callee was " &
                   Type_Decl_Location'Image (Callee));
  end if;


  Start_Test (1003);  -- remote
  A1_2_Obj.Common_Record_Field := 120;
  Single_Controlling_Operand (
       Root_Tagged_Type'Class(A1_2_Obj),
       1003, Callee);
  if A1_2_Obj.Common_Record_Field /= 127 then
    Report.Failed ("test 1003 expected 127 received" &
                   Integer'Image (A1_2_Obj.Common_Record_Field));
  end if;
  if Callee /= Part_A1_2_Spec then
    Report.Failed ("test 1003 callee was " &
                   Type_Decl_Location'Image (Callee));
  end if;


  Start_Test (1004);  -- remote
  A2_Obj.Common_Record_Field := 130;
  A2_Obj.A2_Component := "12345678901234567890";
  Single_Controlling_Operand (
       Root_Tagged_Type'Class(A2_Obj),
       1004, Callee);
  if A2_Obj.Common_Record_Field /= 138 then
    Report.Failed ("test 1004 expected 138 received" &
                   Integer'Image (A2_Obj.Common_Record_Field));
  end if;
  if Callee /= Part_A2_Spec then
    Report.Failed ("test 1004 callee was " &
                   Type_Decl_Location'Image (Callee));
  end if;


  Start_Test (1005);
  B_Obj.Common_Record_Field := 140;
  Single_Controlling_Operand (
       Root_Tagged_Type'Class(B_Obj),
       1005, Callee);
  if B_Obj.Common_Record_Field /= 149 then
    Report.Failed ("test 1005 expected 149 received" &
                   Integer'Image (B_Obj.Common_Record_Field));
  end if;
  if Callee /= Part_B_Spec then
    Report.Failed ("test 1005 callee was " &
                   Type_Decl_Location'Image (Callee));
  end if;

exception
  when others =>
     Report.Failed ("unexpected exception in Dispatching_Test");
end Dispatching_Test;

-----------------------
--
--      Check that tagged types can be passed between partitions
--      when passed as a class-wide type.

procedure Class_Wide_Test is
  Root_Obj   : CXE4006_Common.Root_Tagged_Type;
  A1_1_Obj   : CXE4006_Part_A1.A1_Tagged_Type_1;
  A1_2_Obj   : CXE4006_Part_A1.A1_Tagged_Type_2;
  A2_Obj     : CXE4006_Part_A2.A2_Tagged_Type;
  B_Obj      : CXE4006_Part_B.B_Tagged_Type;

  Callee     : Type_Decl_Location;
begin
  if Verbose then
    Report.Comment ("starting class wide test");
  end if;

  Start_Test (2001);
  Root_Obj.Common_Record_Field := 200;
  CXE4006_Part_A2.Call_B (Root_Obj, 2001, Callee);
  if Root_Obj.Common_Record_Field /= 205 then
    Report.Failed ("test 2001 expected 205 received" &
                   Integer'Image (Root_Obj.Common_Record_Field));
  end if;
  if Callee /= Common_Spec then
    Report.Failed ("test 2001 callee was " &
                   Type_Decl_Location'Image (Callee));
  end if;


  Start_Test (2002);
  A1_1_Obj.Common_Record_Field := 210;
  CXE4006_Part_A2.Call_B (A1_1_Obj, 2002, Callee);
  if A1_1_Obj.Common_Record_Field /= 216 then
    Report.Failed ("test 2002 expected 216 received" &
                   Integer'Image (A1_1_Obj.Common_Record_Field));
  end if;
  if Callee /= Part_A1_1_Spec then
    Report.Failed ("test 2002 callee was " &
                   Type_Decl_Location'Image (Callee));
  end if;


  Start_Test (2003);  -- remote
  A1_2_Obj.Common_Record_Field := 220;
  CXE4006_Part_A2.Call_B (A1_2_Obj, 2003, Callee);
  if A1_2_Obj.Common_Record_Field /= 227 then
    Report.Failed ("test 2003 expected 227 received" &
                   Integer'Image (A1_2_Obj.Common_Record_Field));
  end if;
  if Callee /= Part_A1_2_Spec then
    Report.Failed ("test 2003 callee was " &
                   Type_Decl_Location'Image (Callee));
  end if;


  Start_Test (2004);  -- remote
  A2_Obj.Common_Record_Field := 230;
  A2_Obj.A2_Component := "24680135790987654321";
  CXE4006_Part_A2.Call_B (A2_Obj, 2004, Callee);
  if A2_Obj.Common_Record_Field /= 238 then
    Report.Failed ("test 2004 expected 238 received" &
                   Integer'Image (A2_Obj.Common_Record_Field));
  end if;
  if Callee /= Part_A2_Spec then
    Report.Failed ("test 2004 callee was " &
                   Type_Decl_Location'Image (Callee));
  end if;


  Start_Test (2005);
  B_Obj.Common_Record_Field := 240;
  CXE4006_Part_A2.Call_B (B_Obj, 2005, Callee);
  if B_Obj.Common_Record_Field /= 249 then
    Report.Failed ("test 2005 expected 249 received" &
                   Integer'Image (B_Obj.Common_Record_Field));
  end if;
  if Callee /= Part_B_Spec then
    Report.Failed ("test 2005 callee was " &
                   Type_Decl_Location'Image (Callee));
  end if;

end Class_Wide_Test;

-----------------------
--
--      In a remote subprogram call with a formal parameter of a
--      class-wide type, check that Program_Error is raised if the
--      actual parameter identifies a tagged type not declared in a
--      pure, shared passive, or the visible part of a remote types or
--      remote call interface package.

procedure Class_Wide_Exception_Test is
  Normal_Obj : CXE4006_Normal.Normal_Spec_Tagged_Type;
  Callee     : Type_Decl_Location;
begin
  if Verbose then
    Report.Comment ("starting class wide exception test");
  end if;
  CXE4006_Part_A2.Call_B (Normal_Obj, 3001, Callee);
  Report.Failed ("Program_Error expected but did not occur - test 3001");
exception
  when Program_Error =>  -- expected exception
    if Verbose then
      Report.Comment ("Program_Error raised as expected - test 3001");
    end if;
  when others =>
    Report.Failed ("Program_Error expected but some other exception" &
                   " was raised instead - test 3001");
end Class_Wide_Exception_Test;

-----------------------

begin  -- CXE4006_B
  Report.Test ("CXE4006_B", "Remote dispatching calls");

  -- make sure partitioning was performed correctly

  if CXE4006_Part_A1'Partition_ID = CXE4006_B'Partition_ID then
    Report.Failed ("Partitioning Error - CXE4006_Part_A1 and CXE4006_B" &
                   " are in the same partition.");
  end if;
  if CXE4006_Part_A2'Partition_ID = CXE4006_B'Partition_ID then
    Report.Failed ("Partitioning Error - CXE4006_Part_A2 and CXE4006_B" &
                   " are in the same partition.");
  end if;
  if CXE4006_Part_B'Partition_ID /= CXE4006_B'Partition_ID then
    Report.Failed ("Partitioning Error - CXE4006_Part_B and CXE4006_B" &
                   " are not in the same partition.");
  end if;

  -- do the tests
  Dispatching_Test;
  Class_Wide_Test;
  Class_Wide_Exception_Test;

  -- finish up
  CXE4006_Part_A1.Quit;
  Report.Result;
end CXE4006_B;
