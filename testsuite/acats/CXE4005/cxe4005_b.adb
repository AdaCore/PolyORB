------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                            C X E 4 0 0 5 _ B                             --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--           Copyright (C) 2012, Free Software Foundation, Inc.             --
--                                                                          --
-- This is free software;  you can redistribute it  and/or modify it  under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  This software is distributed in the hope  that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License for  more details.                                               --
--                                                                          --
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
--                                                                          --
--                  PolyORB is maintained by AdaCore                        --
--                     (email: sales@adacore.com)                           --
--                                                                          --
------------------------------------------------------------------------------

-----------------------------------------------------------------------------

with CXE4005_Common;  use CXE4005_Common;
with CXE4005_Normal;
with CXE4005_Part_A1;
with CXE4005_Part_A2;
with CXE4005_Part_B;
with Report;
procedure CXE4005_B is

  -- table of remote access values to all the objects of interest.
  -- Given this table, we can select a remote access value based upon
  -- the type of the object and where the access attribute was evaluated.
  type Pointer_Table_Type is array (Access_Evaluation, Type_Selection) of
      CXE4005_Part_A1.RACWT;
  Pointers : Pointer_Table_Type;

  -- table of serial numbers for the objects used in the Pointers table.
  -- Note that the serial numbers follow the convention that the hundreds
  -- place indicates the package where the object is declared and the
  -- least significant digit indicates the type of the object.
  type Object_SN_Table_Type is array (Access_Evaluation, Type_Selection) of
      Integer;
  Objects : Object_SN_Table_Type := (
     A1 => (101, 106, 107, 108),
     A2 => (201, 206, 207, 208),
     B  => (301, 306, 307, 308));

  Test_Number : Integer := 100;

begin  -- CXE4005_B
  Report.Test ("CXE4005_B", "Remote dispatching calls");

  -- make sure partitioning was performed correctly
  if CXE4005_Part_A1'Partition_ID = CXE4005_B'Partition_ID then
    Report.Failed ("Partitioning Error - CXE4005_Part_A1 and CXE4005_B" &
                   " are in the same partition.");
  end if;
  if CXE4005_Part_A2'Partition_ID = CXE4005_B'Partition_ID then
    Report.Failed ("Partitioning Error - CXE4005_Part_A2 and CXE4005_B" &
                   " are in the same partition.");
  end if;
  if CXE4005_Part_B'Partition_ID /= CXE4005_B'Partition_ID then
    Report.Failed ("Partitioning Error - CXE4005_Part_B and CXE4005_B" &
                   " are not in the same partition.");
  end if;

  -- initialize the table of all access values

  for TS in Type_Selection loop
    Pointers (A1, TS) := CXE4005_Part_A1.Get_RACWT (TS);
    Pointers (A2, TS) := CXE4005_Part_A2.Get_RACWT (TS);
    Pointers (B,  TS) := CXE4005_Part_B.Get_RACWT (TS);
  end loop;

  -- Check the legal calls
  -- This is done by calling Single_Controlling_Operand with
  -- all the legal remote access to class wide type values we have
  -- in the Pointers table and check that the serial number of the object
  -- reported back is the expected value.
  -- Dual_Controlling_Operands is also called with both operands
  -- being the same.
  declare
    SN1 : Integer;
    SN2 : Integer;
  begin
    for AE in Access_Evaluation loop
      for TS in Common_Spec .. RT_Spec loop
        Test_Number := Test_Number + 1;

        if Verbose then
          Report.Comment ("Test" & Integer'Image (Test_Number) &
                          "  Object SN" &
                          Integer'Image (Objects (AE, TS)));
        end if;

        Single_Controlling_Operand (Pointers (AE, TS), Test_Number, SN1);
        if SN1 /= Objects (AE, TS) then
          Report.Failed ("Wrong object used in test number" &
                         Integer'Image (Test_Number) &
                         "  Expected" &
                         Integer'Image (Objects (AE, TS)) &
                         "  Received" &
                         Integer'Image (SN1) &
                         " Single_Controlling_Operands SN" );
        end if;

        Dual_Controlling_Operands  (Pointers (AE, TS), 
                                    Pointers (AE, TS), 
                                    Test_Number, 
                                    SN1,
                                    SN2);
        if SN1 /= Objects (AE, TS) then
          Report.Failed ("Wrong object used in test number" &
                         Integer'Image (Test_Number) &
                         "  Expected" &
                         Integer'Image (Objects (AE, TS)) &
                         "  Received" &
                         Integer'Image (SN1) &
                         " Dual_Controlling_Operands SN1" );
        end if;
        if SN2 /= Objects (AE, TS) then
          Report.Failed ("Wrong object used in test number" &
                         Integer'Image (Test_Number) &
                         "  Expected" &
                         Integer'Image (Objects (AE, TS)) &
                         "  Received" &
                         Integer'Image (SN2) &
                         " Dual_Controlling_Operands SN2" );
        end if;
      end loop;
    end loop;
  exception
    when others => Report.Failed ("Unexpected exception during test" &
                                  Integer'Image (Test_Number));
  end;

  
  -- Check that Program_Error is raised if the tag of the actual
  -- parameter identifies a tagged type declared in a normal package.
  -- E.4(18);6.0
  declare
    X : CXE4005_Normal.Open_But_Not_For_Export;
  begin
    X.Field := 1;  
    CXE4005_Part_A1.Takes_Class_Wide (
          CXE4005_Common.Open_Tagged_Type'Class(X));
    Report.Failed ("Program_Error not raised when remote call is" &
                   " made passing a class-wide object where the" &
                   " type was declared in a" &
                   " normal package");
  exception
    when Program_Error =>  -- expected exception
       if Verbose then
         Report.Comment ("Program_Error raised as expected" & 
                   " when a remote call is made passing a class" &
                   " wide object where the type was declared in" &
                   " a normal package");
       end if;
    when others => 
         Report.Failed ("Incorrect exception raised." &
                        " Program_Error was expected" &
                        " when remote call is made passing a class" &
                        " wide object where the type was declared in" &
                        " a normal package");
  end;

  -- Check that Program_Error is raised if the tag of the actual
  -- parameter identifies a tagged type declared in the body of a
  -- remote call interface package.
  begin
    CXE4005_Part_A1.Takes_Class_Wide (CXE4005_Part_A1.
        Return_Open_Tagged_Type_Class);
    Report.Failed ("Program_Error not raised when remote access to" &
                   " class wide type designated type declared in a" &
                   " package body");
  exception
    when Program_Error =>  -- expected exception
       if Verbose then
         Report.Comment ("Program_Error raised as expected" & 
                   " when remote access to" &
                   " class wide type designated type declared in a" &
                   " package body");
       end if;
    when others => 
         Report.Failed ("Incorrect exception raised." &
                        " Program_Error was expected" &
                        " when remote access to" &
                        " class wide type designated type declared in a" &
                        " package body");
  end;

  -- Check that in a dispatching call with two controlling operands
  -- where the two remote access-to-class-wide values originated
  -- from Access attribute_references in different partitions that
  -- Constraint_Error is raised. 
  declare
    SN1 : Integer;
    SN2 : Integer;
  begin
    Test_Number := 400;
    Dual_Controlling_Operands (Pointers (A1, Common_Spec),
                               Pointers (B,  Common_Spec),
                               Test_Number, SN1, SN2);
    Report.Failed ("Constraint_Error not raised when remote access to" &
                   " class wide type originated from different partitions");
  exception
    when Constraint_Error =>  -- expected exception
       if Verbose then
         Report.Comment ("Constraint_Error raised as expected" & 
                   " when remote access to" &
                   " class wide type originated from different partitions");
       end if;
    when others => 
         Report.Failed ("Incorrect exception raised." &
                        " Constraint_Error was expected");
  end;

  -- Check that in a dispatching call with two controlling operands
  -- where the two remote access-to-class-wide values originated
  -- from Access attribute_references in the same partition but 
  -- different RCI packages that no exception is raised. 
  declare
    SN1 : Integer;
    SN2 : Integer;
  begin
    Test_Number := 500;
    Dual_Controlling_Operands (Pointers (A1, Common_Spec),
                               Pointers (A2, Common_Spec),
                               Test_Number, SN1, SN2);
    if SN1 /= Objects (A1, Common_Spec) then
          Report.Failed ("Wrong object used in test number" &
                         Integer'Image (Test_Number) &
                         "  Expected" &
                         Integer'Image (Objects (A1, Common_Spec)) &
                         "  Received" &
                         Integer'Image (SN1) &
                         " Dual_Controlling_Operands SN1" );
    end if;
    if SN2 /= Objects (A2, Common_Spec) then
          Report.Failed ("Wrong object used in test number" &
                         Integer'Image (Test_Number) &
                         "  Expected" &
                         Integer'Image (Objects (A2, Common_Spec)) &
                         "  Received" &
                         Integer'Image (SN2) &
                         " Dual_Controlling_Operands SN2" );
    end if;
    if Verbose then
      Report.Comment ("Two controlling operands from different RCI" &
                     " packages within the same partition ok");
    end if;
  exception
    when others => 
         Report.Failed ("Two controlling operands from different RCI" &
                        " packages within the same partition" &
                        " resulted in an unexpected exception");
  end;

  -- finish up
  CXE4005_Part_A1.Quit;
  Report.Result;
end CXE4005_B;
