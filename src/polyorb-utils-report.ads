------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                 P O L Y O R B . U T I L S . R E P O R T                  --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--         Copyright (C) 2003-2006, Free Software Foundation, Inc.          --
--                                                                          --
-- PolyORB is free software; you  can  redistribute  it and/or modify it    --
-- under terms of the  GNU General Public License as published by the  Free --
-- Software Foundation;  either version 2,  or (at your option)  any  later --
-- version. PolyORB is distributed  in the hope that it will be  useful,    --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License  for more details.  You should have received  a copy of the GNU  --
-- General Public License distributed with PolyORB; see file COPYING. If    --
-- not, write to the Free Software Foundation, 51 Franklin Street, Fifth    --
-- Floor, Boston, MA 02111-1301, USA.                                       --
--                                                                          --
-- As a special exception,  if other files  instantiate  generics from this --
-- unit, or you link  this unit with other files  to produce an executable, --
-- this  unit  does not  by itself cause  the resulting  executable  to  be --
-- covered  by the  GNU  General  Public  License.  This exception does not --
-- however invalidate  any other reasons why  the executable file  might be --
-- covered by the  GNU Public License.                                      --
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
