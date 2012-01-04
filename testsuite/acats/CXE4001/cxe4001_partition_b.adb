------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                  C X E 4 0 0 1 _ P A R T I T I O N _ B                   --
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
