------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                       T E S T _ S U I T E . R U N                        --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--            Copyright (C) 2004 Free Software Foundation, Inc.             --
--                                                                          --
-- PolyORB is free software; you  can  redistribute  it and/or modify it    --
-- under terms of the  GNU General Public License as published by the  Free --
-- Software Foundation;  either version 2,  or (at your option)  any  later --
-- version. PolyORB is distributed  in the hope that it will be  useful,    --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License  for more details.  You should have received  a copy of the GNU  --
-- General Public License distributed with PolyORB; see file COPYING. If    --
-- not, write to the Free Software Foundation, 59 Temple Place - Suite 330, --
-- Boston, MA 02111-1307, USA.                                              --
--                                                                          --
-- As a special exception,  if other files  instantiate  generics from this --
-- unit, or you link  this unit with other files  to produce an executable, --
-- this  unit  does not  by itself cause  the resulting  executable  to  be --
-- covered  by the  GNU  General  Public  License.  This exception does not --
-- however invalidate  any other reasons why  the executable file  might be --
-- covered by the  GNU Public License.                                      --
--                                                                          --
--                PolyORB is maintained by ACT Europe.                      --
--                    (email: sales@act-europe.fr)                          --
--                                                                          --
------------------------------------------------------------------------------

with GNAT.Expect;
with Test_Suite.Test_Case;
with Test_Suite.Output;

package Test_Suite.Run is

   use GNAT.Expect;
   use Test_Suite.Test_Case;
   use Test_Suite.Output;

   type Analyze_CB is access function (First_Arg : String) return Boolean;

   type Analyze_CB_Array is array (Positive range <>) of Analyze_CB;

   function Run
     (Output           : Test_Suite_Output'Class;
      Exe              : Executable;
      Exec_In_Base_Dir : Boolean;
      First_Arg        : String;
      Item_To_Match    : Regexp_Array;
      Call_Backs       : Analyze_CB_Array;
      Timeout          : Integer)
     return Boolean;
   --  Run executable Exe and output information through
   --  Output. First_Arg is the first argument to be specified when
   --  launching Exe, prior to its other arguments. If the output of
   --  Exe matches the ith item in Item_To_Match, then run the ith
   --  call backs in Call_Backs.

   --  Helper functions

   function Parse_Success (First_Arg : String) return Boolean;
   --  Always return True

   function Parse_Failure (First_Arg : String) return Boolean;
   --  Always return False

end Test_Suite.Run;
