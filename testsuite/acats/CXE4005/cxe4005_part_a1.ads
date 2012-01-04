------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                      C X E 4 0 0 5 _ P A R T _ A 1                       --
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

-----------------------------------------------------------------------------

with CXE4005_Common;  use CXE4005_Common;
package CXE4005_Part_A1 is
  pragma Remote_Call_Interface;
 
  type RACWT is access all CXE4005_Common.Root_Tagged_Type'Class;

  -- provide remote access values to other partitions
  function Get_RACWT (Which_Type : Type_Selection) 
       return CXE4005_Part_A1.RACWT;

  -- for checking E.4(18);6.0.
  procedure Takes_Class_Wide (X : CXE4005_Common.Open_Tagged_Type'Class);
  function Return_Open_Tagged_Type_Class
    return CXE4005_Common.Open_Tagged_Type'Class;

  -- coordination of test termination across partitions
  procedure Can_Quit;
  procedure Quit;

end CXE4005_Part_A1;
