------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                            C X E 1 0 0 1 _ B                             --
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
------------------------------------------------------------------------

with Report;
with System;
with CXE1001_P;      -- a procedure who's ID is to be checked
with CXE1001_Q;      -- a procedure who's ID is to be checked

procedure CXE1001_B is

   type Hold_Partition_ID is range System.Min_Int..System.Max_Int;
   P_ID : Hold_Partition_ID; 
   Q_ID : Hold_Partition_ID; 

begin

   Report.Test ("CXE1001_B", "Check Partition IDs. " &
                                 "-- This is the SECOND PARTITION");

   CXE1001_P;
   CXE1001_Q;
   P_ID := CXE1001_P'Partition_ID;
   Q_ID := CXE1001_Q'Partition_ID;
   if P_ID /= Q_ID then 
      Report.Failed ("Partition IDs of the procedures in this " &
                     "partition are not the same");
   end if;
   Report.Special_Action ("Partition ID of SECOND Partition is: " &
                           Hold_Partition_ID'image(P_ID) & 
                           ".  Check that this is different from that " &
                           "of the FIRST partition");

   Report.Result;

end CXE1001_B;
