------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                            C X E 4 0 0 2 _ B                             --
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

pragma Style_Checks (Off);
-----------------------------------------------------------------------------

with CXE4002_Common;
with CXE4002_Part_A1;
with CXE4002_Part_A2;
with Report;
procedure CXE4002_B is
  function "="(L,R : CXE4002_Common.Integer_Vector) return Boolean
      renames CXE4002_Common."=";
  function "+"(L,R : CXE4002_Common.Integer_Vector) 
      return CXE4002_Common.Integer_Vector
      renames CXE4002_Part_A1."+";
  function "="(L,R : CXE4002_Common.Little_Number) return Boolean
      renames CXE4002_Common."=";

  use type CXE4002_Common.Record_Data;
begin
  Report.Test ("CXE4002_B", "Parameter passing across partitions");

  -- make sure partitioning is performed
  if CXE4002_Part_A1'Partition_ID = CXE4002_B'Partition_ID then
    Report.Failed ("Partitioning Error - CXE4002_Part_A1 and CXE4002_B" &
                   " are in the same partition.");
  end if;

  -- do the tests

  -- simple IN parameter test
  CXE4002_Part_A1.Check_In (1, 2.0, 3);

  -- simple OUT and IN OUT parameter test
  declare
    A : CXE4002_Common.Little_Number;
    B : Float;
    C : Integer;
  begin
    CXE4002_Part_A1.Set_Out (A, B, C);
    if A /= 4 or B /= -123.0 or C /= -789 then
      Report.Failed ("OUT parameters not set properly");
    end if;

    A := 6;
    B := 2.0;
    C := -1;
    CXE4002_Part_A1.Decr (A, B, C);
    if A = 5 and B = 1.0 and C = -2 then
      null;
      -- Report.Comment ("finished simple parameter passing");
    else
      Report.Failed ("IN OUT parameters not returned properly");
    end if;
  end;

  -- do the record type tests now
  declare
    I_Data : CXE4002_Common.Record_Data;
  begin
    I_Data := CXE4002_Part_A1.Current_Record ("1234567890");
    if I_Data /= (1, 198.0, "1234567890") then
      Report.Failed ("composite function result not the expected value");
    end if;

    CXE4002_Part_A1.Update_Record ((22, 33.0, "abcdefghij"), I_Data);
    if I_Data.Part_No /= 24 then
      Report.Failed ("OUT parameter Part_No component has wrong value");
    end if;
    if I_Data.Cost /= 66.0 then
      Report.Failed ("OUT parameter Cost component has wrong value");
    end if;
    if I_Data.Name /= "ABCDEFGHIJ" then
      Report.Failed ("OUT parameter Name component has wrong value");
    else
      null;
      -- Report.Comment ("OUT parameter tests");
    end if;
  end;

  -- do the array type tests now
  declare
    Ones  : constant CXE4002_Common.Integer_Vector := (others => 1);
    Twos  : constant CXE4002_Common.Integer_Vector := (others => 2);
    Fives : constant CXE4002_Common.Integer_Vector := (others => 5);
    Result : CXE4002_Common.Integer_Vector := (others => 0);
  begin
    Result :=  (Twos + Ones) + Twos;
    if Result = Fives then
      null;
      -- Report.Comment ("array parameter and function result");
    else
      Report.Failed ("incorrect array parameters and/or" &
                     " array function results");
    end if;

    Result := Ones;
    CXE4002_Part_A1.Incr_Vector (Result);
    if Result /= Twos then
      Report.Failed ("incorrect array IN OUT parameter");
    end if;
  end;

  -- access to remote subprogram tests
  -- here we make sure the correct procedure is called by having
  -- several procedures with the same parameter profile but each
  -- procedure expects a different value to be passed to it as is
  -- indicated by the procedure name.
  declare
    P2, P3, P4 : CXE4002_Part_A2.Remote_Proc;
  begin
    P2 := CXE4002_Part_A2.Call_With_2'Access;
    P3 := CXE4002_Part_A2.Call_With_3'Access;
    P4 := CXE4002_Part_A1.Call_With_4'Access;
    -- try two different procedures from the same RCI package 
    P2(2);
    P3(3);
    -- try a procedure that is in a different RCI package
    P4(4);
  end;

  -- access to remote subprogram tests with mixed parameters.  
  -- make sure the pointer is used.
  declare
    M1 : CXE4002_Part_A2.Remote_Proc_Mixed;
    M2 : CXE4002_Part_A2.Remote_Proc_Mixed;
    T  : CXE4002_Part_A2.Remote_Proc_Mixed;

    D, E : Integer := 33;

  begin
    T := CXE4002_Part_A2.Mixed_1'Access;
    if Report.Ident_Int (1) = 1 then
      M1 := T;
      M2 := CXE4002_Part_A2.Mixed_2'Access;
    else -- not executed
      M2 := T;
      M1 := CXE4002_Part_A2.Mixed_2'Access;
    end if;
    E := 30;
    M1(20, D, E);
    if D /= 25   or
       E /= 35   then
      Report.Failed ("OUT parameters from Mixed 1 are not the" &
                     " expected values");
    end if;

    E := 300;
    D := 100;
    M2 (200, D, E);
    if D /= 250   or
       E /= 350   then
      Report.Failed ("OUT parameters from Mixed 2 are not the" &
                     " expected values");
    end if;    
  end;

  -- finish up
  CXE4002_Part_A1.Quit;
  Report.Result;
end CXE4002_B;
