------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                 P O L Y O R B . U T I L S . R E P O R T                  --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--         Copyright (C) 2003-2012, Free Software Foundation, Inc.          --
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

--  This package provides utility functions to display example and
--  testsuite outputs, and manipulate some statistical data.

package PolyORB.Utils.Report is

   procedure New_Test (Test_Name : String);
   --  Begin a new test

   procedure Output (Message : String; Result : Boolean);
   --  Output a formatted string with message and the result

   procedure End_Report;
   --  Close a report, returning FALSE if at least one test failed,
   --  TRUE otherwise.

   generic
      type T is delta <>;

   package Statistics is

      type Stat_Vector is array (Natural range <>) of T;

      function Min (V : Stat_Vector) return T;
      --  Return the minimum of statistical vector V

      function Max (V : Stat_Vector) return T;
      --  Return the maximum of statistical vector V

      function Avg (V : Stat_Vector) return Float;
      --  Return the average value of statistical vector V

      function Std_Dev (V : Stat_Vector) return Float;
      --  Return the standard deviation of statistical vector V

      procedure To_GNUPlot (V : Stat_Vector; Filename : String);
      --  Output V as a file ready for GNUPlot, this file will be called
      --  'Filename'.gnuplot. When running 'gnuplot filename.gnuplot',
      --  'Filename'.eps is created.

      type Bin is record
         Value : Natural := 0;
         Index : T;
      end record;

      type Partitions is array (Natural range <>) of Bin;

      function Partition
        (V : Stat_Vector;
         Number_Of_Bins : Natural;
         Low : Float;
         High : Float)
        return Partitions;
      --  Partition V into a set of Number_Of_Bins bins, data are
      --  considered inside the [Low; High] interval.

      procedure To_GNUPlot (P : Partitions; Filename : String);
      --  Output V as a file ready for GNUPlot, this file will be called
      --  'Filename.gnuplot'.

      procedure Analyse_Vector (V : Stat_Vector; Filename : String);
      --  Output statistiacal information about V, store them in 'Filename'

   end Statistics;

end PolyORB.Utils.Report;
