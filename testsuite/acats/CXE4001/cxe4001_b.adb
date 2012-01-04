------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                            C X E 4 0 0 1 _ B                             --
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

with CXE4001_Partition_B;
with CXE4001_Partition_A;
with Report;
with System.RPC;
procedure CXE4001_B is
begin
  Report.Test ("CXE4001_B", "Server partition of exception handling test");
  if CXE4001_Partition_A'Partition_ID = CXE4001_Partition_B'Partition_ID then
    Report.Failed ("Partitioning Error - 1 and Part_B are in the" &
                   " same partition.");
  end if;
  -- Report.Result is called in the body of CXE4001_Partition_B.
end CXE4001_B;
