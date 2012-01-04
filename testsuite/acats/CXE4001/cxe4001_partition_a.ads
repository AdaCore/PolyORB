------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                  C X E 4 0 0 1 _ P A R T I T I O N _ A                   --
--                                                                          --
--                                 S p e c                                  --
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
