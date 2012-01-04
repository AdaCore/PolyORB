------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                       C X E 2 0 0 1 _ P A R T _ B                        --
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

with Report;
with CXE2001_Shared;
package body CXE2001_Part_B is
  task Keep_Partition_Alive is
    entry Quit;
  end Keep_Partition_Alive;

  task body Keep_Partition_Alive is
  begin
    accept Quit;
  end Keep_Partition_Alive;

  procedure Test_Finished is
  begin
    Keep_Partition_Alive.Quit;
    Report.Result;
  end Test_Finished;

  procedure Set_Shared_Data (Value : Integer) is
  begin
    CXE2001_Shared.Shared_Data := Value;
  end Set_Shared_Data;

  procedure Increment_Counter is
  begin
    CXE2001_Shared.Shared_Counter.Increment;
  end Increment_Counter;
end CXE2001_Part_B;
