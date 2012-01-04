------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                          B E N C H _ U T I L S                           --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--         Copyright (C) 2007-2012, Free Software Foundation, Inc.          --
--                                                                          --
-- This is free software;  you can redistribute it  and/or modify it  under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  This software is distributed in the hope  that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License for  more details.                                               --
--                                                                          --
-- As a special exception under Section 7 of GPL version 3, you are granted --
-- additional permissions described in the GCC Runtime Library Exception,   --
-- version 3.1, as published by the Free Software Foundation.               --
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

package Bench_Utils is

   generic
      Test_Name : String;
      with procedure Test;
   procedure Run_Test;
   --  Run_Test will run Test for a limited amount of time, set by
   --  Test_Duration. Run_Test will run as many time as possible Test.
   --
   --  To analyse the ouput and allow for regression tracking,
   --  Run_Test will store all results in a file, named
   --  "Test_Name.data".
      --
   --  Its format is as follows, suitable for processing by GNUPlot:
   --
   --  Date of the run   Time to execute Test  Number of time Test was run
   --  07/09/10:12h55        8.22551E-01        1
   --
   --  The Threshold value is a percentage value that controls the
   --  allowed difference between the current performance and the
   --  latest one.
   --
   --  If there is an overrun, the test is marked as failed,
   --  the output string is as follows:
   --
   --  Performance decreased, old value was 224....................: FAILED

   Test_Duration : Integer := 10;
   --  Test_Duration is, in second, the maximum amount of time allowed
   --  to run test. Run_Test will stop when the execution of N
   --  occurences of Test overruns this value.

   Threshold : Float := 0.00;
   --  Controls the allowed performance difference between the current
   --  run, and the previous one. The test fails if
   --
   --    Old_Value - New_Value > Threshold * New_Value

end Bench_Utils;
