------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                      C X E 4 0 0 2 _ P A R T _ A 1                       --
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
pragma Warnings (Off);
-----------------------------------------------------------------------------

with Report;
with CXE4002_Common;
package body CXE4002_Part_A1 is
  function "+"(X,Y : Little_Number) return Little_Number renames
      CXE4002_Common."+";
  function "-"(X,Y : Little_Number) return Little_Number renames
      CXE4002_Common."-";
  function "="(X,Y : Little_Number) return Boolean renames
      CXE4002_Common."=";

  -- simple integer and float tests
  procedure Check_In (Little : in Little_Number;
                      Real   : in Float;
                      Int    : in Integer) is
  begin
    if Little /= 1 or
       Real   /= 2.0 or
       Int    /= 3 then
      Report.Failed ("incorrect value in mode IN integer and float test");
    else
      null;
      Report.Comment ("mode in integer and float test");
    end if;
  end Check_In;

  procedure Set_Out  (Little : out Little_Number;
                      Real   : out Float;
                      Int    : out Integer) is
  begin
    Report.Comment ("mode out little, integer and float test");
    Little := 4;
    Real := -123.0;
    Int := -789;
  end Set_Out;

  procedure Decr     (Little : in out Little_Number;
                      Real   : in out Float;
                      Int    : in out Integer) is
  begin
    if Little /= 6   or
       Real   /= 2.0 or
       Int    /= -1  then
      Report.Failed ("mode IN OUT parameters did not have the " &
                     " correct value upon entry");
    end if;
    Little := Little - 1;
    Real := Real - 1.0;
    Int := Int - 1;
  end Decr;

  -- Record tests
  function Current_Record (Name : Description) return Record_Data is
  begin
    if Name /= "1234567890" then
      Report.Failed ("string parameter did not have the correct value" &
                     " upon entry");
    end if;
    return (1, 198.0, Name);
  end Current_Record;

  procedure Update_Record (Old_Data : in Record_Data;
                           New_Data : out Record_Data) is
  begin
    New_Data.Part_No := Old_Data.Part_No + 2;
    New_Data.Cost := Old_Data.Cost * 2.0;
    New_Data.Name := "ABCDEFGHIJ";
  end Update_Record;

  -- vector operation tests
  function "+" (A, B : in Integer_Vector) return Integer_Vector is
    Result : Integer_Vector;
  begin
    for I in Integer_Vector'Range loop
      Result (I) := A(I) + B(I);
    end loop;
    return Result;
  end "+";

  procedure Incr_Vector (X : in out Integer_Vector) is
  begin
    for I in Integer_Vector'Range loop
      X (I) := X (I) + 1;
    end loop;
  end Incr_Vector;

  -- remote call test 
  procedure Call_With_4 (T : Integer) is
  begin
    if T /= 4 then
      Report.Failed ("expected 4 but received" & Integer'Image (T));      
    end if;
  end;

  ---------  partition termination coordination ----------
  -- use a task to prevent the partition from completing its execution
  -- until the main procedure in partition B tells it to quit.
  
  task Wait_For_Quit is
    entry Can_Quit;
    entry Quit;
  end Wait_For_Quit;

  task body Wait_For_Quit is
  begin
    accept Can_Quit;
    accept Quit;
    Report.Result;
  end Wait_For_Quit;

  procedure Can_Quit is
  begin
    Wait_For_Quit.Can_Quit;
  end Can_Quit;

  procedure Quit is
  begin
    Wait_For_Quit.Quit;
  end Quit;

end CXE4002_Part_A1;
