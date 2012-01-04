------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                      C X E 4 0 0 2 _ P A R T _ A 1                       --
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
-----------------------------------------------------------------------------

with CXE4002_Common;
package CXE4002_Part_A1 is
  pragma Remote_Call_Interface;

  -- for convenience, rename the imported types used for parameters
  subtype Little_Number  is CXE4002_Common.Little_Number;
  subtype Integer_Vector is CXE4002_Common.Integer_Vector;
  subtype Description    is CXE4002_Common.Description;
  subtype Record_Data    is CXE4002_Common.Record_Data;

  -- simple integer and float tests
  procedure Check_In (Little : in Little_Number;
                      Real   : in Float;
                      Int    : in Integer);
  procedure Set_Out  (Little : out Little_Number;
                      Real   : out Float;
                      Int    : out Integer);
  procedure Decr     (Little : in out Little_Number;
                      Real   : in out Float;
                      Int    : in out Integer);

  -- record tests
  function Current_Record (Name : Description) return Record_Data;
  procedure Update_Record (Old_Data : in Record_Data;
                           New_Data : out Record_Data);

  -- array tests
  function "+"(A, B : in Integer_Vector) return Integer_Vector;
  procedure Incr_Vector (X : in out Integer_Vector);

  -- access test support
  procedure Call_With_4 (T : Integer);

  -- coordination of test termination across partitions
  procedure Can_Quit;
  procedure Quit;
end CXE4002_Part_A1;
