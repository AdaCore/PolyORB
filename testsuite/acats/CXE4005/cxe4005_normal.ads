------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                       C X E 4 0 0 5 _ N O R M A L                        --
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

with CXE4005_Common;
package CXE4005_Normal is
  type Cant_Use_In_Remote_Call is new CXE4005_Common.Root_Tagged_Type
       with null record;

  procedure Single_Controlling_Operand (
      RTT         : access Cant_Use_In_Remote_Call;
      Test_Number : in     Integer;
      Obj_SN      : out    Integer);
  procedure Dual_Controlling_Operands (
      RTT1        : access Cant_Use_In_Remote_Call;
      RTT2        : access Cant_Use_In_Remote_Call;
      Test_Number : in     Integer;
      Obj_SN1     : out    Integer;
      Obj_SN2     : out    Integer);

  type Open_But_Not_For_Export is new CXE4005_Common.Open_Tagged_Type
      with null record;
end CXE4005_Normal;
